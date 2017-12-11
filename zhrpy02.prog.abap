REPORT ZHRPY02.
"薪资汇总表
TABLES: PERNR,
        CSKT,
        MCS0.
TYPES: BEGIN OF TY_OUT ,
         "统计纬度
         ABKRS       TYPE PERNR-ABKRS, "工资范围
         JSMON       TYPE MCS0-SPMON, "计算年月
         KOSTL       TYPE PA0001-KOSTL , "成本中心
         A0_OBJ      TYPE PA0001-ORGEH , "集团编号
         B0_OBJ      TYPE PA0001-ORGEH , "控股公司编号
         C1_OBJ      TYPE PA0001-ORGEH , "一级编号
         C2_OBJ      TYPE PA0001-ORGEH , "二级编号
         C3_OBJ      TYPE PA0001-ORGEH , "三级编号
         C4_OBJ      TYPE PA0001-ORGEH , "四级编号

         KOSTL_KTEXT TYPE KTEXT, "成本中心文本
         A0          TYPE STRING , "集团组织名称
         B0          TYPE STRING,  "控股组织名称
         C1          TYPE STRING,  "一级组织名称
         C2          TYPE STRING, "二级组织名称
         C3          TYPE STRING, "三级组织名称
         C4          TYPE STRING, "四级组织名称

         GZMON       TYPE MCS0-SPMON, "工资年月


         ATEXT       TYPE STRING,              "工资核算范围文本
         PERNR       TYPE PERNR-PERNR,         "人员编号
         DYJCGZ      TYPE MAXBT , "当月基础工资
         DYGWGZ      TYPE MAXBT, "当月岗位工资
         JXGZBZ      TYPE MAXBT, "绩效工资标准 1030
         JXXS        TYPE MAXBT, "绩效系数 1031
         DYJXGZ      TYPE MAXBT, "当月绩效工资
         YWF         TYPE MAXBT, "业务费
         DYBTGZ      TYPE MAXBT, "当月补贴工资
         GWJT        TYPE MAXBT, "岗位津贴
         JXJT        TYPE MAXBT, "绩效津贴
         JNJT        TYPE MAXBT, "技能津贴
         JJGZ        TYPE MAXBT, "计件工资
         JJBFJ       TYPE MAXBT, "计件补发金
         TSGWBT      TYPE MAXBT, "特殊岗位补贴
         ZFBZ01      TYPE MAXBT, "住房补助 （70小时内） 3061
         ZFBZ02      TYPE MAXBT, "住房补贴(70及以上) 3065
         ZFBZ        TYPE MAXBT, "住房补助
         CFBZ        TYPE MAXBT, "餐费补助
         GWBZ        TYPE MAXBT, "高温补助
         CCBZ        TYPE MAXBT, "出差补助
         HFBZ        TYPE MAXBT, "话费补助
         GLJ         TYPE MAXBT, "工龄奖
         QQJ         TYPE MAXBT, "全勤奖
         DYXMBZ      TYPE MAXBT, "当月项目补助
         JXKHZJ      TYPE MAXBT, "绩效考核增减
         JBBZ        TYPE MAXBT, "加班补助
         YBJT        TYPE MAXBT, "夜班津贴
         JXGZYKJ     TYPE MAXBT, "绩效工资预扣减
         DNBT        TYPE MAXBT, "电脑补贴
         SSZBT       TYPE MAXBT, "宿舍长补贴
         GKBZ        TYPE MAXBT, "高空补助
         XSTC        TYPE MAXBT, "销售提成
         KZJJ        TYPE MAXBT, "考证奖金
         QTJJ        TYPE MAXBT, "其他奖金
         LBYPJE      TYPE MAXBT, "劳保用品金额
         JRFL        TYPE MAXBT, "节日福利
         TSFLJ       TYPE MAXBT, "特殊福利金
         SQQT        TYPE MAXBT, "税前其他
         PSJBF       TYPE MAXBT, "平时加班费
         ZMJBF       TYPE MAXBT, "周末加班费
         FDJJRJBF    TYPE MAXBT, "法定节假日加班费
         SJKK        TYPE MAXBT, "事假扣款
         BJKK        TYPE MAXBT, "病假扣款
         KGKK01      TYPE MAXBT, "旷工扣款（光电金达）-显示
         KGKK02      TYPE MAXBT, "旷工扣款（电视深圳利亚德）-显示
         KGKK        TYPE MAXBT, "旷工扣款
         YCKK        TYPE MAXBT, "迟到/早退/打卡异常扣款
         ZJE         TYPE MAXBT, "总金额 /101
         YFGZZE      TYPE MAXBT, "应发工资总额
         GJJ_GR      TYPE MAXBT, "公积金-个人
         YLAOBX_GR   TYPE MAXBT, "养老保险-个人
         SYBX_GR     TYPE MAXBT, "失业保险-个人
         YLIAOBX_GR  TYPE MAXBT, "医疗保险-个人
         DBYLBX_GR   TYPE MAXBT, "大病医疗保险-个人
         FDFLKFHJ_GR TYPE  MAXBT, "法定福利个人扣发合计
         GJJ_GS      TYPE MAXBT, "公积金-公司
         YLAOBX_GS   TYPE MAXBT, "养老保险-公司
         SYEBX_GS    TYPE MAXBT, "失业保险-公司
         YLIAO_GS    TYPE MAXBT, "医疗保险公司
         DBYLBX_GS   TYPE MAXBT, "大病医疗保险-公司
         GSBX_GS     TYPE MAXBT, "工伤保险-公司
         SYUBX_GS    TYPE MAXBT, "生育保险-公司
         FDFLHJ_GS   TYPE MAXBT, "法定福利公司合计
         XJJS        TYPE MAXBT, "现金计税
         YSGZZE      TYPE MAXBT, "应税工资总额
         GRSDS       TYPE MAXBT, "个人所得税
         JSHGZZE     TYPE MAXBT, "计税后工资总额
         SSZSF       TYPE MAXBT, "宿舍住宿费
         SSSDF       TYPE MAXBT, "宿舍水电费
         GSFK        TYPE MAXBT, "公司罚款
         JK          TYPE MAXBT, "借款
         GYJK        TYPE MAXBT, "公益捐款
         CRMKK       TYPE MAXBT, "CRM扣款
         CFKJ        TYPE MAXBT, "餐费扣减
         SJQFKJ      TYPE MAXBT, "手机欠费扣减
         LZGYF       TYPE MAXBT, "离职工衣费
         DNBZJ       TYPE MAXBT, "电脑保证金
         SHQT        TYPE MAXBT, "税后其他
         SHHJZE      TYPE MAXBT, "税后合计后总额
         BFGZ        TYPE MAXBT, "补发工资
         HFGZ        TYPE MAXBT, "缓发工资
         NZJJ        TYPE MAXBT, "年终奖金
         NZJS        TYPE MAXBT, "年终奖税
         LZBCJ       TYPE MAXBT, "离职补偿金
         LZBCJS      TYPE MAXBT, "离职补偿金税
         DYSFGZ      TYPE MAXBT, "当月实发工资
         BYQK        TYPE MAXBT, "本月欠款  /561
         QYQK        TYPE MAXBT, "前月欠款  /563
         CYFLG       TYPE C,                   "差异

*&--代码添加 BY HANDYBY 07.06.2017 13:54:59  BEGIN
         YGYGZ       TYPE MAXBT , "员工月工资
         JTBZ        TYPE MAXBT,  "交通补助
         BTZS        TYPE MAXBT,  "补贴折算
         BTZXHJ      TYPE MAXBT,  "补贴杂项合计
         WQBZ        TYPE MAXBT,  "外勤补助
         WPBZ        TYPE MAXBT,  "外派补助
         XMBZ        TYPE MAXBT,  "项目补助
         YJJJ        TYPE MAXBT,  "业绩奖金
         WCBT        TYPE MAXBT,  "午餐补贴
         JXJJ        TYPE MAXBT,  "绩效奖金
         QTKK        TYPE MAXBT,  "其它扣款
         SHHFKK      TYPE MAXBT,  "税后话费扣款
         ZDJBYLBZ_GR TYPE MAXBT,  "重大疾病医疗补助（个人）
         SBTZ_GR     TYPE MAXBT,  "社保调整（个人）
         GJJTZ_GR    TYPE MAXBT,  "公积金调整（个人）
         ZDJBYLBZ_GS TYPE MAXBT,  "重大疾病医疗补助（公司）
         YWBX_GS     TYPE MAXBT,  "意外保险（公司）
*&--代码添加 BY HANDYBY 07.06.2017 13:54:59  END

*&--代码添加 BY HANDYBY 12.06.2017 10:01:13  BEGIN
         XLBT_JD     TYPE MAXBT, "学历补贴（金达）
         ZYZGBT_JD   TYPE MAXBT, "执业资格补贴（金达）
         JSZCBT_JD   TYPE MAXBT, "技术职称补贴（金达）
         JTBT_JD     TYPE MAXBT, "交通补贴（金达）
         HFBT_JD     TYPE MAXBT, "话费补贴（金达）
         CFBZ_JD     TYPE MAXBT, "餐费补助（金达）
         ZFBT_JD     TYPE MAXBT, "住房补贴（金达）
*&--代码添加 BY HANDYBY 12.06.2017 10:01:13  END

*&--代码添加 BY HANDYBY 29.06.2017 20:04:57  BEGIN
         GLJT        TYPE MAXBT,  "管理津贴
         ZWJT        TYPE MAXBT,  "职位津贴
         JSJT        TYPE MAXBT,  "技术津贴
         FJJBXZ      TYPE MAXBT,  "附加基本薪资
         BTJT        TYPE MAXBT,  "补贴津贴
*&--代码添加 BY HANDYBY 29.06.2017 20:04:57  END

       END OF  TY_OUT,

       "汇总字段
       BEGIN OF  TY_OUT_TOT,
         ABKRS       TYPE PERNR-ABKRS, "工资范围
         JSMON       TYPE MCS0-SPMON, "计算年月
         XH          TYPE INT4,          "序号
         KOSTL       TYPE PA0001-KOSTL , "成本中心
         A0_OBJ      TYPE PA0001-ORGEH , "集团编号
         B0_OBJ      TYPE PA0001-ORGEH , "控股公司编号
         C1_OBJ      TYPE PA0001-ORGEH , "一级编号
         C2_OBJ      TYPE PA0001-ORGEH , "二级编号
         C3_OBJ      TYPE PA0001-ORGEH , "三级编号
         C4_OBJ      TYPE PA0001-ORGEH , "四级编号

         ATEXT       TYPE STRING,              "工资核算范围文本
         KOSTL_KTEXT TYPE KTEXT, "成本中心文本
         A0          TYPE STRING , "集团组织名称
         B0          TYPE STRING,  "控股组织名称
         C1          TYPE STRING,  "一级组织名称
         C2          TYPE STRING, "二级组织名称
         C3          TYPE STRING, "三级组织名称
         C4          TYPE STRING, "四级组织名称
         ZRS         TYPE INT4,         "总人数
         DYJCGZ      TYPE MAXBT , "当月基础工资
         DYGWGZ      TYPE MAXBT, "当月岗位工资
         JXGZBZ      TYPE MAXBT, "绩效工资标准 1030
         JXXS        TYPE MAXBT, "绩效系数 1031
         DYJXGZ      TYPE MAXBT, "当月绩效工资
         YWF         TYPE MAXBT, "业务费
         DYBTGZ      TYPE MAXBT, "当月补贴工资
         GWJT        TYPE MAXBT, "岗位津贴
         JXJT        TYPE MAXBT, "绩效津贴
         JNJT        TYPE MAXBT, "技能津贴
         JJGZ        TYPE MAXBT, "计件工资
         JJBFJ       TYPE MAXBT, "计件补发金
         TSGWBT      TYPE MAXBT, "特殊岗位补贴
         ZFBZ01      TYPE MAXBT, "住房补助 （70小时内） 3061
         ZFBZ02      TYPE MAXBT, "住房补贴(70及以上) 3065
         ZFBZ        TYPE MAXBT, "住房补助
         CFBZ        TYPE MAXBT, "餐费补助
         GWBZ        TYPE MAXBT, "高温补助
         CCBZ        TYPE MAXBT, "出差补助
         HFBZ        TYPE MAXBT, "话费补助
         GLJ         TYPE MAXBT, "工龄奖
         QQJ         TYPE MAXBT, "全勤奖
         DYXMBZ      TYPE MAXBT, "当月项目补助
         JXKHZJ      TYPE MAXBT, "绩效考核增减
         JBBZ        TYPE MAXBT, "加班补助
         YBJT        TYPE MAXBT, "夜班津贴
         JXGZYKJ     TYPE MAXBT, "绩效工资预扣减
         DNBT        TYPE MAXBT, "电脑补贴
         SSZBT       TYPE MAXBT, "宿舍长补贴
         GKBZ        TYPE MAXBT, "高空补助
         XSTC        TYPE MAXBT, "销售提成
         KZJJ        TYPE MAXBT, "考证奖金
         QTJJ        TYPE MAXBT, "其他奖金
         LBYPJE      TYPE MAXBT, "劳保用品金额
         JRFL        TYPE MAXBT, "节日福利
         TSFLJ       TYPE MAXBT, "特殊福利金
         SQQT        TYPE MAXBT, "税前其他
         PSJBF       TYPE MAXBT, "平时加班费
         ZMJBF       TYPE MAXBT, "周末加班费
         FDJJRJBF    TYPE MAXBT, "法定节假日加班费
         SJKK        TYPE MAXBT, "事假扣款
         BJKK        TYPE MAXBT, "病假扣款
         KGKK01      TYPE MAXBT, "旷工扣款（光电金达）-显示
         KGKK02      TYPE MAXBT, "旷工扣款（电视深圳利亚德）-显示
         KGKK        TYPE MAXBT, "旷工扣款
         YCKK        TYPE MAXBT, "迟到/早退/打卡异常扣款
         ZJE         TYPE MAXBT, "总金额 /101
         YFGZZE      TYPE MAXBT, "应发工资总额
         GJJ_GR      TYPE MAXBT, "公积金-个人
         YLAOBX_GR   TYPE MAXBT, "养老保险-个人
         SYBX_GR     TYPE MAXBT, "失业保险-个人
         YLIAOBX_GR  TYPE MAXBT, "医疗保险-个人
         DBYLBX_GR   TYPE MAXBT, "大病医疗保险-个人
         FDFLKFHJ_GR TYPE  MAXBT, "法定福利个人扣发合计
         GJJ_GS      TYPE MAXBT, "公积金-公司
         YLAOBX_GS   TYPE MAXBT, "养老保险-公司
         SYEBX_GS    TYPE MAXBT, "失业保险-公司
         YLIAO_GS    TYPE MAXBT, "医疗保险公司
         DBYLBX_GS   TYPE MAXBT, "大病医疗保险-公司
         GSBX_GS     TYPE MAXBT, "工伤保险-公司
         SYUBX_GS    TYPE MAXBT, "生育保险-公司
         FDFLHJ_GS   TYPE MAXBT, "法定福利公司合计
         XJJS        TYPE MAXBT, "现金计税
         YSGZZE      TYPE MAXBT, "应税工资总额
         GRSDS       TYPE MAXBT, "个人所得税
         JSHGZZE     TYPE MAXBT, "计税后工资总额
         SSZSF       TYPE MAXBT, "宿舍住宿费
         SSSDF       TYPE MAXBT, "宿舍水电费
         GSFK        TYPE MAXBT, "公司罚款
         JK          TYPE MAXBT, "借款
         GYJK        TYPE MAXBT, "公益捐款
         CRMKK       TYPE MAXBT, "CRM扣款
         CFKJ        TYPE MAXBT, "餐费扣减
         SJQFKJ      TYPE MAXBT, "手机欠费扣减
         LZGYF       TYPE MAXBT, "离职工衣费
         DNBZJ       TYPE MAXBT, "电脑保证金
         SHQT        TYPE MAXBT, "税后其他
         SHHJZE      TYPE MAXBT, "税后合计后总额
         BFGZ        TYPE MAXBT, "补发工资
         HFGZ        TYPE MAXBT, "缓发工资
         NZJJ        TYPE MAXBT, "年终奖金
         NZJS        TYPE MAXBT, "年终奖税
         LZBCJ       TYPE MAXBT, "离职补偿金
         LZBCJS      TYPE MAXBT, "离职补偿金税
         DYSFGZ      TYPE MAXBT, "当月实发工资
         BYQK        TYPE MAXBT, "本月欠款  /561
         QYQK        TYPE MAXBT, "前月欠款  /563
         SEL,

*&--代码添加 BY HANDYBY 07.06.2017 13:54:59  BEGIN
         YGYGZ       TYPE MAXBT , "员工月工资
         JTBZ        TYPE MAXBT,  "交通补助
         BTZS        TYPE MAXBT,  "补贴折算
         BTZXHJ      TYPE MAXBT,  "补贴杂项合计
         WQBZ        TYPE MAXBT,  "外勤补助
         WPBZ        TYPE MAXBT,  "外派补助
         XMBZ        TYPE MAXBT,  "项目补助
         YJJJ        TYPE MAXBT,  "业绩奖金
         WCBT        TYPE MAXBT,  "午餐补贴
         JXJJ        TYPE MAXBT,  "绩效奖金
         QTKK        TYPE MAXBT,  "其它扣款
         SHHFKK      TYPE MAXBT,  "税后话费扣款
         ZDJBYLBZ_GR TYPE MAXBT,  "重大疾病医疗补助（个人）
         SBTZ_GR     TYPE MAXBT,  "社保调整（个人）
         GJJTZ_GR    TYPE MAXBT,  "公积金调整（个人）
         ZDJBYLBZ_GS TYPE MAXBT,  "重大疾病医疗补助（公司）
         YWBX_GS     TYPE MAXBT,  "意外保险（公司）
*&--代码添加 BY HANDYBY 07.06.2017 13:54:59  END

*&--代码添加 BY HANDYBY 12.06.2017 10:01:13  BEGIN
         XLBT_JD     TYPE MAXBT, "学历补贴（金达）
         ZYZGBT_JD   TYPE MAXBT, "执业资格补贴（金达）
         JSZCBT_JD   TYPE MAXBT, "技术职称补贴（金达）
         JTBT_JD     TYPE MAXBT, "交通补贴（金达）
         HFBT_JD     TYPE MAXBT, "话费补贴（金达）
         CFBZ_JD     TYPE MAXBT, "餐费补助（金达）
         ZFBT_JD     TYPE MAXBT, "住房补贴（金达）
*&--代码添加 BY HANDYBY 12.06.2017 10:01:13  END

       END   OF TY_OUT_TOT.

TYPES:BEGIN OF GST_ZZCJ,
        ORGEH TYPE C LENGTH 8, "部门层级
        SHORT TYPE SHORT_D,
        BMRS  TYPE I,
      END OF GST_ZZCJ .
DATA:GS_ZZCJ   TYPE GST_ZZCJ.
DATA:GT_ZZCJ   TYPE STANDARD TABLE OF GST_ZZCJ.
"***  define internal table & work area
DATA: GT_OUT     TYPE STANDARD TABLE OF TY_OUT,
      GW_OUT     TYPE                   TY_OUT,
      GT_OUT_TOT TYPE STANDARD TABLE OF TY_OUT_TOT,
      GW_OUT_TOT TYPE                   TY_OUT_TOT,
      GT_PA0001  TYPE STANDARD TABLE OF PA0001,
      GT_PA0000  TYPE STANDARD TABLE OF PA0000,
      GT_PA0041  TYPE STANDARD TABLE OF PA0041,
      GT_PA0185  TYPE STANDARD TABLE OF PA0185,
      GT_PA0008  TYPE STANDARD TABLE OF PA0008,
      GT_PA9128  TYPE STANDARD TABLE OF PA9128,
      GT_HRP1001 TYPE STANDARD TABLE OF HRP1001,

      "定义文本内表
      GT_T501T   TYPE STANDARD TABLE OF T501T,     "员工组文本
      GT_T503T   TYPE STANDARD TABLE OF T503T,     "员工子组文本
      GT_T527X   TYPE STANDARD TABLE OF T527X,     "组织单位文本
      GT_T529U   TYPE STANDARD TABLE OF T529U,     "雇佣状态文本
      GT_HRP1000 TYPE STANDARD TABLE OF HRP1000,   "组织岗位文本
      GT_T500P   TYPE STANDARD TABLE OF T500P,     "人事范围文本
      GT_T510G   TYPE STANDARD TABLE OF T510G,     "工资等级范围（工资体系)
      GT_T549T   TYPE STANDARD TABLE OF T549T ,   "工资核算范围
      GT_CSKT    TYPE STANDARD TABLE OF CSKT .    "成本中心

***  define alv internal table & work area.
DATA: GT_FCAT       TYPE LVC_T_FCAT,
      W_FCAT        TYPE LVC_S_FCAT,
      W_LAYOUT      TYPE LVC_S_LAYO,
      GT_EVENTS     TYPE SLIS_T_EVENT,
      W_EVENT       TYPE SLIS_ALV_EVENT,
      GT_CELLSTYL   TYPE STANDARD TABLE OF LVC_S_STYL,
      W_CELLSTYL    LIKE LINE OF GT_CELLSTYL,
      GT_EVENT_EXIT TYPE SLIS_T_EVENT_EXIT,
      W_EVENT_EXIT  TYPE SLIS_EVENT_EXIT.
DATA: ALV_VARIANT    TYPE DISVARIANT.                       "#EC NEEDED

***  define globle data
DATA: G_BEGDA TYPE SY-DATUM,                   "开始日期
      G_ENDDA TYPE SY-DATUM,                   "结束日期
      G_TEXT  TYPE STRING.                     "部门标识

CONSTANTS: C_OBJ_PRE  TYPE STRING VALUE '_OBJ'.

RANGES: S_ORGEH_SEL  FOR PA0001-ORGEH.

***  define field-symbols
FIELD-SYMBOLS: <FS_PA0001>  TYPE PA0001,
               <FS_PA0000>  TYPE PA0000,
               <FS_PA0185>  TYPE PA0185,
               <FS_PA0041>  TYPE PA0041,
               <FS_T501T>   TYPE T501T,
               <FS_T503T>   TYPE T503T,
               <FS_T549T>   TYPE T549T,
               <FS_T527X>   TYPE T527X,
               <FS_T529U>   TYPE T529U,
               <FS_HRP1000> TYPE HRP1000,
               <FS_HRP1001> TYPE HRP1001,
               <FS_PA0008>  TYPE PA0008,
               <FS_T510G>   TYPE T510G,
               <FS_PA9128>  TYPE PA9128,
               <FS_T500P>   TYPE T500P,
               <FS_CSKT>    TYPE CSKT.

***  define selection-screen
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: S_SPMON FOR MCS0-SPMON  OBLIGATORY  ,"NO-EXTENSION NO INTERVALS, "薪资期间
                S_ABKRS FOR PERNR-ABKRS,     "工资范围
                S_ORGEH FOR PERNR-ORGEH NO-EXTENSION NO INTERVALS ."OBLIGATORY.      "部门
*                S_PERSG FOR PERNR-PERSG.
PARAMETERS: P_PAYTY TYPE PAYTY,                                          "工资核算类型
            P_PAYID TYPE PAYID.                                          "工资核算标识
*SELECT-OPTIONS: S_WERKS FOR PERNR-WERKS.                                     "人事范围

PARAMETERS: P_QJN TYPE C RADIOBUTTON GROUP G2 DEFAULT 'X',               "期间内视图
            P_LJQ TYPE C RADIOBUTTON GROUP G2 .                          "历经期视图

SELECTION-SCREEN END   OF BLOCK BLK1.

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-WD1.

"成本中心
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_KOSTL TYPE C RADIOBUTTON GROUP G1  DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(10) TEXT-002.
SELECTION-SCREEN POSITION 15.
"PARAMETERS P_TKSTL TYPE CHAR02 DEFAULT 'A0'.
SELECTION-SCREEN END OF LINE.


"集团
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_G1 TYPE C RADIOBUTTON GROUP G1 .
SELECTION-SCREEN COMMENT 3(10) TEXT-003.
*SELECTION-SCREEN POSITION 15.
*PARAMETERS P_T1 TYPE CHAR02 DEFAULT 'A0'.
SELECTION-SCREEN END OF LINE.

"控股公司
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_G2 TYPE C RADIOBUTTON GROUP G1.
SELECTION-SCREEN COMMENT 3(10) TEXT-004.
*SELECTION-SCREEN POSITION 15.
*PARAMETERS P_T2 TYPE CHAR02 DEFAULT 'B0'.
SELECTION-SCREEN END OF LINE.

"一级部门
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_G3 TYPE C RADIOBUTTON GROUP G1.
SELECTION-SCREEN COMMENT 3(10) TEXT-005.
*SELECTION-SCREEN POSITION 15.
*PARAMETERS P_T3 TYPE CHAR02 DEFAULT 'C1'.
SELECTION-SCREEN END OF LINE.

"二级部门
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_G4 TYPE C RADIOBUTTON GROUP G1.
SELECTION-SCREEN COMMENT 3(10) TEXT-006.
*SELECTION-SCREEN POSITION 15.
*PARAMETERS P_T4 TYPE CHAR02 DEFAULT 'C2'.
SELECTION-SCREEN END OF LINE.

"三级部门
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_G5 TYPE C RADIOBUTTON GROUP G1.
SELECTION-SCREEN COMMENT 3(10) TEXT-007.
*SELECTION-SCREEN POSITION 15.
*PARAMETERS P_T5 TYPE CHAR02 DEFAULT 'C3'.
SELECTION-SCREEN END OF LINE.
"四级部门
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_G6 TYPE C RADIOBUTTON GROUP G1.
SELECTION-SCREEN COMMENT 3(10) TEXT-008.
*SELECTION-SCREEN POSITION 15.
*PARAMETERS P_T6 TYPE CHAR02 DEFAULT 'C4'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END   OF BLOCK BLK2.

*SELECTION-SCREEN: BEGIN OF BLOCK ANZ WITH FRAME TITLE TEXT-ZZZ.
*PARAMETERS: ALV_DEF     LIKE DISVARIANT-VARIANT.
*SELECTION-SCREEN: END OF BLOCK ANZ.
*** INITIALIZATION
INITIALIZATION.

*** SCREEN PBO
AT SELECTION-SCREEN OUTPUT.
  PERFORM PBO.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR ALV_DEF.
*  PERFORM ALV_F4.

*** AT SELECTION-SCREEN
AT SELECTION-SCREEN.

  PERFORM ALV_CHECK.
*** search help 添加 选择组织结构的搜索帮助
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORGEH-LOW. "
  PERFORM HROBJID_HELP.
*** START-OF-SELECTION
START-OF-SELECTION.

  "检查数据
  PERFORM CHECK_DATA.

  "获取人员数据
  PERFORM GET_PERSON_DATA.

  "处理数据
  PERFORM  PROCESS_DATA.

  "填充字段目录
  PERFORM INITIAL_FIELDCAT.

  "填充格式
  PERFORM INITIAL_LAYOUT.

  PERFORM DISPLAY_ALV.
*&---------------------------------------------------------------------*
*&      Form  PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PBO .
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_QJN' OR
       SCREEN-NAME = 'P_LJQ'.
      SCREEN-ACTIVE = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_F4 .
* ALV_VARIANT-REPORT = SY-REPID.
*
*  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
*    EXPORTING
*      IS_VARIANT = ALV_VARIANT
*      I_SAVE     = 'A'
*    IMPORTING
*      ES_VARIANT = ALV_VARIANT
*    EXCEPTIONS
*      NOT_FOUND  = 2.
*  IF SY-SUBRC = 2.
*    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ELSE.
*    ALV_DEF = ALV_VARIANT-VARIANT.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_CHECK .
* SET CURSOR                 FIELD  'ALV_DEF'.              "n725824
*
*  ALV_VARIANT-REPORT = SY-REPID.
*  ALV_VARIANT-VARIANT = ALV_DEF.
*
*  IF ALV_DEF IS NOT INITIAL.
*    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
*      EXPORTING
*        I_SAVE     = 'A'
*      CHANGING
*        CS_VARIANT = ALV_VARIANT
*      EXCEPTIONS
*        OTHERS     = 1.
*    IF SY-SUBRC <> 0.
*      MESSAGE E321(M7) WITH ALV_DEF SY-REPID.
*    ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DATA .
  DATA: L_ENDMO   TYPE SPMON.

  CLEAR L_ENDMO.

  "检查薪资期间
  IF S_SPMON-LOW+4(2) < 1 OR S_SPMON-LOW+4(2) > 12.
    "MESSAGE S006 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  "开始日期
  CONCATENATE S_SPMON-LOW '01' INTO G_BEGDA.

  IF S_SPMON-HIGH IS NOT INITIAL.
    IF S_SPMON-HIGH+4(2) < 1 OR S_SPMON-HIGH+4(2) > 12.
      "MESSAGE S006 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    L_ENDMO = S_SPMON-HIGH.
  ELSE.
    L_ENDMO = S_SPMON-LOW.
  ENDIF.

  CLEAR G_TEXT.

  IF P_G1 = 'X'.
    G_TEXT = 'A0'.
  ENDIF.

  IF P_G2 = 'X'.
    G_TEXT = 'B0'.
  ENDIF.

  IF P_G3 = 'X'.
    G_TEXT = 'C1'.
  ENDIF.

  IF P_G4 = 'X'.
    G_TEXT = 'C2'.
  ENDIF.

  IF P_G5 = 'X'.
    G_TEXT = 'C3'.
  ENDIF.

  IF P_G6 = 'X'.
    G_TEXT = 'C4'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_PERSON_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PERSON_DATA .
  "取人员基础信息
  SELECT *
    INTO TABLE GT_PA0001
    FROM PA0001
  WHERE
     ORGEH IN S_ORGEH_SEL "组织编号
    AND
    ABKRS IN S_ABKRS.              "工资范围

  IF GT_PA0001[] IS INITIAL.
    "MESSAGE S004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  "取人员状态
  SELECT *
    INTO TABLE GT_PA0000
    FROM PA0000
    FOR ALL ENTRIES IN GT_PA0001
  WHERE PERNR = GT_PA0001-PERNR.

*  "取基本工资信息
*  SELECT *
*    INTO TABLE GT_PA0008
*    FROM PA0008
*    FOR ALL ENTRIES IN GT_PA0001
*  WHERE PERNR = GT_PA0001-PERNR.

***********  以下取文本信息     *******************************

  "取组织岗位文本
  SELECT *
    INTO TABLE GT_HRP1000
    FROM HRP1000
  WHERE PLVAR = '01'                "计划版本
    AND OTYPE IN ('O')      "对象类型
    AND ISTAT = '1'.                "计划状态

  "取组织岗位文本
  SELECT *
    INTO TABLE GT_HRP1001
    FROM HRP1001
    FOR ALL ENTRIES IN GT_HRP1000
  WHERE OBJID = GT_HRP1000-OBJID "编号
    AND PLVAR = '01'                "计划版本
    AND OTYPE IN ('O')      "对象类型
    AND ISTAT = '1'                 "计划状态
    AND RSIGN = 'A'                 "关系规格
    AND RELAT = '002'.              "对象间的关系

  "取工资核算范围的文本
  SELECT *
    INTO TABLE GT_T549T
    FROM T549T
  WHERE SPRSL = '1'.

  "取成本中心
  SELECT *
    INTO TABLE GT_CSKT
    FROM CSKT
    WHERE SPRAS = '1'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA .
  DATA: L_ZRS    TYPE INT4.
  DATA: L_XH     TYPE INT4.
  DATA: ZZMS     TYPE STRING.
  FIELD-SYMBOLS: <LFS_PA0001> TYPE PA0001,
                 <LFS_OUT>    TYPE TY_OUT.

  SORT GT_PA0001   BY PERNR.
  SORT GT_HRP1000  BY OBJID.
  SORT GT_HRP1001  BY OBJID.

  "遍历人员数据
  LOOP AT GT_PA0001 ASSIGNING <LFS_PA0001>.

    AT NEW PERNR.

************ 调用子例程获取工资信息   *********************

      PERFORM PROCESS_PAY_DATA USING <LFS_PA0001>-PERNR.

************ 调用子例程获取工资信息   *********************
    ENDAT.

  ENDLOOP.

  IF GT_OUT[] IS INITIAL.
    "MESSAGE S004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF P_KOSTL = 'X'.
    SORT GT_OUT BY ABKRS  JSMON  KOSTL ."按成本中心排序
  ELSE.
    SORT GT_OUT BY ABKRS JSMON  A0_OBJ B0_OBJ C1_OBJ C2_OBJ C3_OBJ  C4_OBJ. "按 组织层级排序
  ENDIF.

  LOOP AT GT_OUT  ASSIGNING  <LFS_OUT>.

    IF P_KOSTL EQ 'X'.
      AT NEW  KOSTL .
        CLEAR:GW_OUT_TOT,
            L_ZRS.
      ENDAT.
    ENDIF.
    IF P_G1 EQ 'X'.
      AT NEW  A0_OBJ.
        CLEAR:GW_OUT_TOT,
            L_ZRS.
      ENDAT.
    ENDIF.
    IF P_G2 EQ 'X'.
      AT NEW  B0_OBJ .
        CLEAR:GW_OUT_TOT,
            L_ZRS.
      ENDAT.
    ENDIF.
    IF P_G3 EQ 'X'.
      AT NEW  C1_OBJ .
        CLEAR:GW_OUT_TOT,
            L_ZRS.
      ENDAT.
    ENDIF.
    IF P_G4 EQ 'X'.
      AT NEW  C2_OBJ .
        CLEAR:GW_OUT_TOT,
            L_ZRS.
      ENDAT.
    ENDIF.
    IF P_G5 EQ 'X'.
      AT NEW  C3_OBJ .
        CLEAR:GW_OUT_TOT,
            L_ZRS.
      ENDAT.
    ENDIF.
    IF P_G6 EQ 'X'.
      AT NEW  C4_OBJ .
        CLEAR:GW_OUT_TOT,
            L_ZRS.
      ENDAT.
    ENDIF.


    "计算总人数
    L_ZRS                 = L_ZRS + 1.

    "当月基础工资
    GW_OUT_TOT-DYJCGZ      = GW_OUT_TOT-DYJCGZ + <LFS_OUT>-DYJCGZ.
    "当月岗位工资
    GW_OUT_TOT-DYGWGZ      = GW_OUT_TOT-DYGWGZ + <LFS_OUT>-DYGWGZ.
    "当月绩效工资
    "GW_OUT_TOT-DYJXGZ      = GW_OUT_TOT-DYJXGZ + <LFS_OUT>-DYJXGZ.
    "------------------------------------------------
    "绩效系数--黄芳增加
    GW_OUT_TOT-JXXS      = GW_OUT_TOT-JXXS + <LFS_OUT>-JXXS.
    "绩效工资标准--黄芳增加
    GW_OUT_TOT-JXGZBZ      = GW_OUT_TOT-JXGZBZ + <LFS_OUT>-JXGZBZ.
    "------------------------------------------------

    "业务费
    GW_OUT_TOT-YWF      = GW_OUT_TOT-YWF + <LFS_OUT>-YWF.
    "当月补贴工资
    GW_OUT_TOT-DYBTGZ      = GW_OUT_TOT-DYBTGZ + <LFS_OUT>-DYBTGZ.
    "岗位津贴
    GW_OUT_TOT-GWJT      = GW_OUT_TOT-GWJT + <LFS_OUT>-GWJT.
    "绩效津贴
    GW_OUT_TOT-JXJT      = GW_OUT_TOT-JXJT + <LFS_OUT>-JXJT.
    "技能津贴
    GW_OUT_TOT-JNJT      = GW_OUT_TOT-JNJT + <LFS_OUT>-JNJT.

*&--代码添加 BY HANDYBY 07.06.2017 14:01:04  BEGIN
    GW_OUT_TOT-YGYGZ      = GW_OUT_TOT-YGYGZ + <LFS_OUT>-YGYGZ.
*&--代码添加 BY HANDYBY 07.06.2017 14:01:04  END

    "计件工资
    GW_OUT_TOT-JJGZ      = GW_OUT_TOT-JJGZ + <LFS_OUT>-JJGZ.
    "计件补发金
    GW_OUT_TOT-JJBFJ      = GW_OUT_TOT-JJBFJ + <LFS_OUT>-JJBFJ.
    "特殊岗位补贴
    GW_OUT_TOT-TSGWBT      = GW_OUT_TOT-TSGWBT + <LFS_OUT>-TSGWBT.

    "餐费补助
    GW_OUT_TOT-CFBZ      = GW_OUT_TOT-CFBZ + <LFS_OUT>-CFBZ.
    "高温补助
    GW_OUT_TOT-GWBZ      = GW_OUT_TOT-GWBZ + <LFS_OUT>-GWBZ.
    "出差补助
    GW_OUT_TOT-CCBZ      = GW_OUT_TOT-CCBZ + <LFS_OUT>-CCBZ.
    "话费补助
    GW_OUT_TOT-HFBZ      = GW_OUT_TOT-HFBZ + <LFS_OUT>-HFBZ.

*&--代码添加 BY HANDYBY 07.06.2017 14:01:04  BEGIN
    GW_OUT_TOT-JTBZ      = GW_OUT_TOT-JTBZ + <LFS_OUT>-JTBZ.
    GW_OUT_TOT-BTZS      = GW_OUT_TOT-BTZS + <LFS_OUT>-BTZS.
*&--代码添加 BY HANDYBY 07.06.2017 14:01:04  END

    "工龄奖
    GW_OUT_TOT-GLJ      = GW_OUT_TOT-GLJ + <LFS_OUT>-GLJ.
    "全勤奖
    GW_OUT_TOT-QQJ      = GW_OUT_TOT-QQJ + <LFS_OUT>-QQJ.
    "当月项目补助
    GW_OUT_TOT-DYXMBZ      = GW_OUT_TOT-DYXMBZ + <LFS_OUT>-DYXMBZ.
    "绩效考核增减
    GW_OUT_TOT-JXKHZJ      = GW_OUT_TOT-JXKHZJ + <LFS_OUT>-JXKHZJ.
    "加班补助
    GW_OUT_TOT-JBBZ      = GW_OUT_TOT-JBBZ + <LFS_OUT>-JBBZ.
    "夜班津贴
    GW_OUT_TOT-YBJT      = GW_OUT_TOT-YBJT + <LFS_OUT>-YBJT.
    "绩效工资预扣减
    GW_OUT_TOT-JXGZYKJ      = GW_OUT_TOT-JXGZYKJ + <LFS_OUT>-JXGZYKJ.
    "电脑补贴
    GW_OUT_TOT-DNBT      = GW_OUT_TOT-DNBT + <LFS_OUT>-DNBT.
    "宿舍长补贴
    GW_OUT_TOT-SSZBT      = GW_OUT_TOT-SSZBT + <LFS_OUT>-SSZBT.
    "高空补助
    GW_OUT_TOT-GKBZ      = GW_OUT_TOT-GKBZ + <LFS_OUT>-GKBZ.
    "销售提成
    GW_OUT_TOT-XSTC      = GW_OUT_TOT-XSTC + <LFS_OUT>-XSTC.

*&--代码添加 BY HANDYBY 07.06.2017 14:01:04  BEGIN
    GW_OUT_TOT-BTZXHJ      = GW_OUT_TOT-BTZXHJ + <LFS_OUT>-BTZXHJ.
    GW_OUT_TOT-WQBZ      = GW_OUT_TOT-WQBZ + <LFS_OUT>-WQBZ.
    GW_OUT_TOT-WPBZ      = GW_OUT_TOT-WPBZ + <LFS_OUT>-WPBZ.
    GW_OUT_TOT-XMBZ      = GW_OUT_TOT-XMBZ + <LFS_OUT>-XMBZ.
    GW_OUT_TOT-YJJJ      = GW_OUT_TOT-YJJJ + <LFS_OUT>-YJJJ.
    GW_OUT_TOT-WCBT      = GW_OUT_TOT-WCBT + <LFS_OUT>-WCBT.
    GW_OUT_TOT-JXJJ      = GW_OUT_TOT-JXJJ + <LFS_OUT>-JXJJ.
    GW_OUT_TOT-QTKK      = GW_OUT_TOT-QTKK + <LFS_OUT>-QTKK.
    GW_OUT_TOT-SHHFKK      = GW_OUT_TOT-SHHFKK + <LFS_OUT>-SHHFKK.
*&--代码添加 BY HANDYBY 07.06.2017 14:01:04  END

    "考证奖金
    GW_OUT_TOT-KZJJ      = GW_OUT_TOT-KZJJ + <LFS_OUT>-KZJJ.
    "其他奖金
    GW_OUT_TOT-QTJJ      = GW_OUT_TOT-QTJJ + <LFS_OUT>-QTJJ.
    "劳保用品金额
    GW_OUT_TOT-LBYPJE      = GW_OUT_TOT-LBYPJE + <LFS_OUT>-LBYPJE.
    "节日福利
    GW_OUT_TOT-JRFL      = GW_OUT_TOT-JRFL + <LFS_OUT>-JRFL.
    "特殊福利金
    GW_OUT_TOT-TSFLJ      = GW_OUT_TOT-TSFLJ + <LFS_OUT>-TSFLJ.
    "税前其他
    GW_OUT_TOT-SQQT      = GW_OUT_TOT-SQQT + <LFS_OUT>-SQQT.
    "平时加班费
    GW_OUT_TOT-PSJBF      = GW_OUT_TOT-PSJBF + <LFS_OUT>-PSJBF.
    "周末加班费
    GW_OUT_TOT-ZMJBF      = GW_OUT_TOT-ZMJBF + <LFS_OUT>-ZMJBF.
    "法定节假日加班费
    GW_OUT_TOT-FDJJRJBF      = GW_OUT_TOT-FDJJRJBF + <LFS_OUT>-FDJJRJBF.
    "事假扣款
    GW_OUT_TOT-SJKK     = GW_OUT_TOT-SJKK + <LFS_OUT>-SJKK.
    "病假扣款
    GW_OUT_TOT-BJKK      = GW_OUT_TOT-BJKK + <LFS_OUT>-BJKK.

    "迟到/早退/打卡异常扣款
    GW_OUT_TOT-YCKK      = GW_OUT_TOT-YCKK + <LFS_OUT>-YCKK.

    "公积金-个人
    GW_OUT_TOT-GJJ_GR      = GW_OUT_TOT-GJJ_GR + <LFS_OUT>-GJJ_GR.
    "养老保险-个人
    GW_OUT_TOT-YLAOBX_GR      = GW_OUT_TOT-YLAOBX_GR + <LFS_OUT>-YLAOBX_GR.
    "失业保险-个人
    GW_OUT_TOT-SYBX_GR     = GW_OUT_TOT-SYBX_GR + <LFS_OUT>-SYBX_GR.
    "医疗保险-个人
    GW_OUT_TOT-YLIAOBX_GR      = GW_OUT_TOT-YLIAOBX_GR + <LFS_OUT>-YLIAOBX_GR.
    "大病医疗保险-个人
    GW_OUT_TOT-DBYLBX_GR      = GW_OUT_TOT-DBYLBX_GR + <LFS_OUT>-DBYLBX_GR.

*&--代码添加 BY HANDYBY 07.06.2017 14:09:26  BEGIN
    GW_OUT_TOT-ZDJBYLBZ_GR      = GW_OUT_TOT-ZDJBYLBZ_GR + <LFS_OUT>-ZDJBYLBZ_GR.
    GW_OUT_TOT-SBTZ_GR      = GW_OUT_TOT-SBTZ_GR + <LFS_OUT>-SBTZ_GR.
    GW_OUT_TOT-GJJTZ_GR      = GW_OUT_TOT-GJJTZ_GR + <LFS_OUT>-GJJTZ_GR.
*&--代码添加 BY HANDYBY 07.06.2017 14:09:26  END

    "       "公积金-公司
    GW_OUT_TOT-GJJ_GS     = GW_OUT_TOT-GJJ_GS + <LFS_OUT>-GJJ_GS.
    "养老保险-公司
    GW_OUT_TOT-YLAOBX_GS      = GW_OUT_TOT-YLAOBX_GS + <LFS_OUT>-YLAOBX_GS.
    "失业保险-公司
    GW_OUT_TOT-SYEBX_GS      = GW_OUT_TOT-SYEBX_GS + <LFS_OUT>-SYEBX_GS.
    "医疗保险公司
    GW_OUT_TOT-YLIAO_GS      = GW_OUT_TOT-YLIAO_GS + <LFS_OUT>-YLIAO_GS.
    "大病医疗保险-公司
    GW_OUT_TOT-DBYLBX_GS      = GW_OUT_TOT-DBYLBX_GS + <LFS_OUT>-DBYLBX_GS.
    "工伤保险-公司
    GW_OUT_TOT-GSBX_GS      = GW_OUT_TOT-GSBX_GS + <LFS_OUT>-GSBX_GS.
*    "养老保险-公司
*    GW_OUT_TOT-YLAOBX_GS      = GW_OUT_TOT-YLAOBX_GS + <LFS_OUT>-YLAOBX_GS.
    "生育保险-公司
    GW_OUT_TOT-SYUBX_GS      = GW_OUT_TOT-SYUBX_GS + <LFS_OUT>-SYUBX_GS.

*&--代码添加 BY HANDYBY 07.06.2017 14:10:21  BEGIN
    GW_OUT_TOT-ZDJBYLBZ_GS      = GW_OUT_TOT-ZDJBYLBZ_GS + <LFS_OUT>-ZDJBYLBZ_GS.
    GW_OUT_TOT-YWBX_GS      = GW_OUT_TOT-YWBX_GS + <LFS_OUT>-YWBX_GS.
*&--代码添加 BY HANDYBY 07.06.2017 14:10:21  END

*&--代码添加 BY HANDYBY 12.06.2017 10:10:29  BEGIN
    GW_OUT_TOT-XLBT_JD      = GW_OUT_TOT-XLBT_JD + <LFS_OUT>-XLBT_JD.
    GW_OUT_TOT-ZYZGBT_JD      = GW_OUT_TOT-ZYZGBT_JD + <LFS_OUT>-ZYZGBT_JD.
    GW_OUT_TOT-JSZCBT_JD      = GW_OUT_TOT-JSZCBT_JD + <LFS_OUT>-JSZCBT_JD.
    GW_OUT_TOT-JTBT_JD      = GW_OUT_TOT-JTBT_JD + <LFS_OUT>-JTBT_JD.
    GW_OUT_TOT-HFBT_JD      = GW_OUT_TOT-HFBT_JD + <LFS_OUT>-HFBT_JD.
    GW_OUT_TOT-CFBZ_JD      = GW_OUT_TOT-CFBZ_JD + <LFS_OUT>-CFBZ_JD.
    GW_OUT_TOT-ZFBT_JD      = GW_OUT_TOT-ZFBT_JD + <LFS_OUT>-ZFBT_JD.
*&--代码添加 BY HANDYBY 12.06.2017 10:10:29  END

    "现金计税
    GW_OUT_TOT-XJJS      = GW_OUT_TOT-XJJS + <LFS_OUT>-XJJS.
    "应税工资总额
    GW_OUT_TOT-YSGZZE      = GW_OUT_TOT-YSGZZE + <LFS_OUT>-YSGZZE.
    "个人所得税
    GW_OUT_TOT-GRSDS      = GW_OUT_TOT-GRSDS + <LFS_OUT>-GRSDS.

    "宿舍住宿费
    GW_OUT_TOT-SSZSF      = GW_OUT_TOT-SSZSF + <LFS_OUT>-SSZSF.
    "宿舍水电费
    GW_OUT_TOT-SSSDF     = GW_OUT_TOT-SSSDF + <LFS_OUT>-SSSDF.
    "公司罚款
    GW_OUT_TOT-GSFK      = GW_OUT_TOT-GSFK + <LFS_OUT>-GSFK.
    "借款
    GW_OUT_TOT-JK     = GW_OUT_TOT-JK + <LFS_OUT>-JK.
    "公益捐款
    GW_OUT_TOT-GYJK     = GW_OUT_TOT-GYJK + <LFS_OUT>-GYJK.
    "CRM扣款
    GW_OUT_TOT-CRMKK     = GW_OUT_TOT-CRMKK + <LFS_OUT>-CRMKK.
    "餐费扣减
    GW_OUT_TOT-CFKJ     = GW_OUT_TOT-CFKJ + <LFS_OUT>-CFKJ.
    "手机欠费扣减
    GW_OUT_TOT-SJQFKJ     = GW_OUT_TOT-SJQFKJ + <LFS_OUT>-SJQFKJ.
    "离职工衣费
    GW_OUT_TOT-LZGYF     = GW_OUT_TOT-LZGYF + <LFS_OUT>-LZGYF.
    "电脑保证金
    GW_OUT_TOT-DNBZJ     = GW_OUT_TOT-DNBZJ + <LFS_OUT>-DNBZJ.
    "税后其他
    GW_OUT_TOT-SHQT     = GW_OUT_TOT-SHQT + <LFS_OUT>-SHQT.

    "补发工资
    GW_OUT_TOT-BFGZ     = GW_OUT_TOT-BFGZ + <LFS_OUT>-BFGZ.
    "缓发工资
    GW_OUT_TOT-HFGZ     = GW_OUT_TOT-HFGZ + <LFS_OUT>-HFGZ.
    "年终奖金
    GW_OUT_TOT-NZJJ     = GW_OUT_TOT-NZJJ + <LFS_OUT>-NZJJ.
    "年终奖税
    GW_OUT_TOT-NZJS     = GW_OUT_TOT-NZJS + <LFS_OUT>-NZJS.
    "离职补偿金
    GW_OUT_TOT-LZBCJ     = GW_OUT_TOT-LZBCJ + <LFS_OUT>-LZBCJ.
    "离职补偿金税
    GW_OUT_TOT-LZBCJS     = GW_OUT_TOT-LZBCJS + <LFS_OUT>-LZBCJS.
    "当月实发工资
    GW_OUT_TOT-DYSFGZ     = GW_OUT_TOT-DYSFGZ + <LFS_OUT>-DYSFGZ.
    "住房补助
    GW_OUT_TOT-ZFBZ01    =  GW_OUT_TOT-ZFBZ01 + <LFS_OUT>-ZFBZ01.
    GW_OUT_TOT-ZFBZ02     =  GW_OUT_TOT-ZFBZ02 + <LFS_OUT>-ZFBZ02.
    GW_OUT_TOT-KGKK01     =  GW_OUT_TOT-KGKK01 + <LFS_OUT>-KGKK01.
    GW_OUT_TOT-KGKK02    =  GW_OUT_TOT-KGKK02 + <LFS_OUT>-KGKK02.
    GW_OUT_TOT-ZJE       =  GW_OUT_TOT-ZJE + <LFS_OUT>-ZJE.
    "本月欠款
    GW_OUT_TOT-BYQK     = GW_OUT_TOT-BYQK + <LFS_OUT>-BYQK.
    "前月欠款
    GW_OUT_TOT-QYQK    = GW_OUT_TOT-QYQK + <LFS_OUT>-QYQK.
    .
    IF P_KOSTL EQ 'X'.
      AT END OF KOSTL .
        IF GW_OUT_TOT IS NOT INITIAL.
          L_XH = L_XH   + 1 .
          GW_OUT_TOT-ZRS      = L_ZRS.
          GW_OUT_TOT-XH       = L_XH .
          GW_OUT_TOT-JSMON    = <LFS_OUT>-JSMON.
          GW_OUT_TOT-ABKRS    = <LFS_OUT>-ABKRS.
          "工资核算范围
          READ TABLE GT_T549T ASSIGNING <FS_T549T> WITH KEY ABKRS = GW_OUT_TOT-ABKRS BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GW_OUT_TOT-ATEXT         = <FS_T549T>-ATEXT.
          ENDIF.
          GW_OUT_TOT-KOSTL    = <LFS_OUT>-KOSTL.
          READ TABLE  GT_CSKT ASSIGNING <FS_CSKT>   WITH KEY KOSTL = GW_OUT_TOT-KOSTL BINARY SEARCH.
          IF SY-SUBRC EQ 0 .
            GW_OUT_TOT-KOSTL_KTEXT  = <FS_CSKT>-KTEXT .
          ENDIF.
*        "住房补助 :3061 + 3065
          GW_OUT_TOT-ZFBZ   = GW_OUT_TOT-ZFBZ + GW_OUT_TOT-ZFBZ01  + GW_OUT_TOT-ZFBZ02 .
          "旷工扣款 ： 5575 + 5576
          GW_OUT_TOT-KGKK = GW_OUT_TOT-KGKK + GW_OUT_TOT-KGKK01 + GW_OUT_TOT-KGKK02.
          "应发工资总额 :/101 - 6160 - 6200

*        "当月绩效工资:1030 + 1031 ---黄芳增加
          GW_OUT_TOT-DYJXGZ    = GW_OUT_TOT-DYJXGZ + GW_OUT_TOT-JXGZBZ + GW_OUT_TOT-JXXS.
          "----------------------黄芳增加-----------------------------------


          GW_OUT_TOT-YFGZZE = GW_OUT_TOT-YFGZZE + ( GW_OUT_TOT-ZJE - GW_OUT_TOT-NZJJ - GW_OUT_TOT-LZBCJ  )  .

          "法定福利个人扣发合计 ：8313+8323+8333+8363+8373
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  BEGIN
*          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
*                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR.
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  END
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  BEGIN
          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR + GW_OUT_TOT-ZDJBYLBZ_GR + GW_OUT_TOT-SBTZ_GR +
                                     GW_OUT_TOT-GJJTZ_GR .
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  END

          "法定福利公司合计 ：8314 + 8324 + 8334 + 8344 + 8354 + 8364 + 8374
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  BEGIN
*         GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
*                            + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS.
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  END
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  BEGIN
          GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
                              + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS +
                              GW_OUT_TOT-ZDJBYLBZ_GS + GW_OUT_TOT-YWBX_GS .
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  END

          "计税后工资总额 :/401 - 6150 - /403
          GW_OUT_TOT-JSHGZZE = GW_OUT_TOT-JSHGZZE + GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS - GW_OUT_TOT-GRSDS .
          "税后合计后总额 :/401-6150 - /403 + 7100 -(7020+7000+7030+7050+7010+7040+7080+7090+7060+7070）
          GW_OUT_TOT-SHHJZE =  GW_OUT_TOT-SHHJZE +  GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS -  GW_OUT_TOT-GRSDS + GW_OUT_TOT-SHQT  - ( GW_OUT_TOT-SSZSF + GW_OUT_TOT-SSSDF +
                           GW_OUT_TOT-GSFK + GW_OUT_TOT-JK + GW_OUT_TOT-GYJK + GW_OUT_TOT-CRMKK + GW_OUT_TOT-CFKJ + GW_OUT_TOT-SJQFKJ
                            + GW_OUT_TOT-LZGYF + GW_OUT_TOT-DNBZJ  ) .
          APPEND GW_OUT_TOT TO GT_OUT_TOT.
        ENDIF.
      ENDAT.
    ENDIF.
    IF P_G1 EQ 'X'.
      AT END OF A0_OBJ .
        IF GW_OUT_TOT IS NOT INITIAL.
          L_XH = L_XH   + 1 .
          GW_OUT_TOT-ZRS      = L_ZRS.
          GW_OUT_TOT-XH       = L_XH .
          GW_OUT_TOT-JSMON    = <LFS_OUT>-JSMON.
          GW_OUT_TOT-ABKRS    = <LFS_OUT>-ABKRS.
          "工资核算范围
          READ TABLE GT_T549T ASSIGNING <FS_T549T> WITH KEY ABKRS = GW_OUT_TOT-ABKRS BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GW_OUT_TOT-ATEXT         = <FS_T549T>-ATEXT.
          ENDIF.
          GW_OUT_TOT-A0_OBJ   = <LFS_OUT>-A0_OBJ.
          "取组织结构的描述
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-A0_OBJ CHANGING  GW_OUT_TOT-A0  .
*               "住房补助 :3061 + 3065
          GW_OUT_TOT-ZFBZ   = GW_OUT_TOT-ZFBZ + GW_OUT_TOT-ZFBZ01  + GW_OUT_TOT-ZFBZ02 .
          "旷工扣款 ： 5575 + 5576
          GW_OUT_TOT-KGKK = GW_OUT_TOT-KGKK + GW_OUT_TOT-KGKK01 + GW_OUT_TOT-KGKK02.

*        "当月绩效工资:1030 + 1031 ---黄芳增加
          GW_OUT_TOT-DYJXGZ    = GW_OUT_TOT-DYJXGZ + GW_OUT_TOT-JXGZBZ + GW_OUT_TOT-JXXS.
          "----------------------黄芳增加-----------------------------------




          "应发工资总额 :/101 - 6160 - 6200
          GW_OUT_TOT-YFGZZE = GW_OUT_TOT-YFGZZE + ( GW_OUT_TOT-ZJE - GW_OUT_TOT-NZJJ - GW_OUT_TOT-LZBCJ  )  .

          "法定福利个人扣发合计 ：8313+8323+8333+8363+8373
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  BEGIN
*          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
*                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR.
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  END
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  BEGIN
          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR + GW_OUT_TOT-ZDJBYLBZ_GR + GW_OUT_TOT-SBTZ_GR +
                                     GW_OUT_TOT-GJJTZ_GR .
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  END

          "法定福利公司合计 ：8314 + 8324 + 8334 + 8344 + 8354 + 8364 + 8374
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  BEGIN
*         GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
*                            + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS.
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  END
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  BEGIN
          GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
                              + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS +
                              GW_OUT_TOT-ZDJBYLBZ_GS + GW_OUT_TOT-YWBX_GS .
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  END

          "计税后工资总额 :/401 - 6150 - /403
          GW_OUT_TOT-JSHGZZE = GW_OUT_TOT-JSHGZZE + GW_OUT_TOT-YSGZZE  - GW_OUT_TOT-XJJS -  GW_OUT_TOT-GRSDS .
          "税后合计后总额 :/401-6150 - /403 + 7100 -(7020+7000+7030+7050+7010+7040+7080+7090+7060+7070）
          GW_OUT_TOT-SHHJZE =  GW_OUT_TOT-SHHJZE +  GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS -  GW_OUT_TOT-GRSDS + GW_OUT_TOT-SHQT  - ( GW_OUT_TOT-SSZSF + GW_OUT_TOT-SSSDF +
                           GW_OUT_TOT-GSFK + GW_OUT_TOT-JK + GW_OUT_TOT-GYJK + GW_OUT_TOT-CRMKK + GW_OUT_TOT-CFKJ + GW_OUT_TOT-SJQFKJ
                            + GW_OUT_TOT-LZGYF + GW_OUT_TOT-DNBZJ  ) .
          APPEND GW_OUT_TOT TO GT_OUT_TOT.
        ENDIF.
      ENDAT.
    ENDIF.

    IF P_G2 EQ 'X'.
      AT END OF B0_OBJ .
        IF GW_OUT_TOT IS NOT INITIAL.
          L_XH = L_XH   + 1 .
          GW_OUT_TOT-ZRS      = L_ZRS.
          GW_OUT_TOT-XH       = L_XH .
          GW_OUT_TOT-JSMON    = <LFS_OUT>-JSMON.
          GW_OUT_TOT-ABKRS    = <LFS_OUT>-ABKRS.
          "工资核算范围
          READ TABLE GT_T549T ASSIGNING <FS_T549T> WITH KEY ABKRS = GW_OUT_TOT-ABKRS BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GW_OUT_TOT-ATEXT         = <FS_T549T>-ATEXT.
          ENDIF.
          GW_OUT_TOT-A0_OBJ   = <LFS_OUT>-A0_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-A0_OBJ CHANGING  GW_OUT_TOT-A0  .
          GW_OUT_TOT-B0_OBJ   = <LFS_OUT>-B0_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-B0_OBJ CHANGING  GW_OUT_TOT-B0  .
*                "住房补助 :3061 + 3065
          GW_OUT_TOT-ZFBZ   = GW_OUT_TOT-ZFBZ + GW_OUT_TOT-ZFBZ01  + GW_OUT_TOT-ZFBZ02 .
          "旷工扣款 ： 5575 + 5576
          GW_OUT_TOT-KGKK = GW_OUT_TOT-KGKK + GW_OUT_TOT-KGKK01 + GW_OUT_TOT-KGKK02.

          "当月绩效工资:1030 + 1031 ---黄芳增加
          GW_OUT_TOT-DYJXGZ    = GW_OUT_TOT-DYJXGZ + GW_OUT_TOT-JXGZBZ + GW_OUT_TOT-JXXS.
          "----------------------黄芳增加-----------------------------------

          "应发工资总额 :/101 - 6160 - 6200
          GW_OUT_TOT-YFGZZE = GW_OUT_TOT-YFGZZE + ( GW_OUT_TOT-ZJE - GW_OUT_TOT-NZJJ - GW_OUT_TOT-LZBCJ  )  .

          "法定福利个人扣发合计 ：8313+8323+8333+8363+8373
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  BEGIN
*          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
*                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR.
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  END
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  BEGIN
          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR + GW_OUT_TOT-ZDJBYLBZ_GR + GW_OUT_TOT-SBTZ_GR +
                                     GW_OUT_TOT-GJJTZ_GR .
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  END

          "法定福利公司合计 ：8314 + 8324 + 8334 + 8344 + 8354 + 8364 + 8374
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  BEGIN
*         GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
*                            + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS.
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  END
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  BEGIN
          GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
                              + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS +
                              GW_OUT_TOT-ZDJBYLBZ_GS + GW_OUT_TOT-YWBX_GS .
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  END

          "计税后工资总额 :/401 - 6150 - /403
          GW_OUT_TOT-JSHGZZE = GW_OUT_TOT-JSHGZZE + GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS - GW_OUT_TOT-GRSDS .
          "税后合计后总额 :/401-6150 - /403 + 7100 -(7020+7000+7030+7050+7010+7040+7080+7090+7060+7070）
          GW_OUT_TOT-SHHJZE =  GW_OUT_TOT-SHHJZE +  GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS -  GW_OUT_TOT-GRSDS + GW_OUT_TOT-SHQT  - ( GW_OUT_TOT-SSZSF + GW_OUT_TOT-SSSDF +
                           GW_OUT_TOT-GSFK + GW_OUT_TOT-JK + GW_OUT_TOT-GYJK + GW_OUT_TOT-CRMKK + GW_OUT_TOT-CFKJ + GW_OUT_TOT-SJQFKJ
                            + GW_OUT_TOT-LZGYF + GW_OUT_TOT-DNBZJ  ) .
          APPEND GW_OUT_TOT TO GT_OUT_TOT.
        ENDIF.
      ENDAT.
    ENDIF.
    IF P_G3 EQ 'X'.
      AT END OF C1_OBJ .
        IF GW_OUT_TOT IS NOT INITIAL.
          L_XH = L_XH   + 1 .
          GW_OUT_TOT-ZRS      = L_ZRS.
          GW_OUT_TOT-XH       = L_XH .
          GW_OUT_TOT-JSMON    = <LFS_OUT>-JSMON.
          GW_OUT_TOT-ABKRS    = <LFS_OUT>-ABKRS.
          "工资核算范围
          READ TABLE GT_T549T ASSIGNING <FS_T549T> WITH KEY ABKRS = GW_OUT_TOT-ABKRS BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GW_OUT_TOT-ATEXT         = <FS_T549T>-ATEXT.
          ENDIF.
          GW_OUT_TOT-A0_OBJ   = <LFS_OUT>-A0_OBJ.
          GW_OUT_TOT-A0_OBJ   = <LFS_OUT>-A0_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-A0_OBJ CHANGING  GW_OUT_TOT-A0  .
          GW_OUT_TOT-B0_OBJ   = <LFS_OUT>-B0_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-B0_OBJ CHANGING  GW_OUT_TOT-B0  .
          GW_OUT_TOT-C1_OBJ   = <LFS_OUT>-C1_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-C1_OBJ CHANGING  GW_OUT_TOT-C1  .
*               "住房补助 :3061 + 3065
          GW_OUT_TOT-ZFBZ   = GW_OUT_TOT-ZFBZ + GW_OUT_TOT-ZFBZ01  + GW_OUT_TOT-ZFBZ02 .
          "旷工扣款 ： 5575 + 5576
          GW_OUT_TOT-KGKK = GW_OUT_TOT-KGKK + GW_OUT_TOT-KGKK01 + GW_OUT_TOT-KGKK02.

          "当月绩效工资:1030 + 1031 ---黄芳增加
          GW_OUT_TOT-DYJXGZ    = GW_OUT_TOT-DYJXGZ + GW_OUT_TOT-JXGZBZ + GW_OUT_TOT-JXXS.
          "----------------------黄芳增加-----------------------------------


          "应发工资总额 :/101 - 6160 - 6200
          GW_OUT_TOT-YFGZZE = GW_OUT_TOT-YFGZZE + ( GW_OUT_TOT-ZJE - GW_OUT_TOT-NZJJ - GW_OUT_TOT-LZBCJ  )  .

          "法定福利个人扣发合计 ：8313+8323+8333+8363+8373
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  BEGIN
*          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
*                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR.
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  END
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  BEGIN
          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR + GW_OUT_TOT-ZDJBYLBZ_GR + GW_OUT_TOT-SBTZ_GR +
                                     GW_OUT_TOT-GJJTZ_GR .
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  END

          "法定福利公司合计 ：8314 + 8324 + 8334 + 8344 + 8354 + 8364 + 8374
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  BEGIN
*         GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
*                            + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS.
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  END
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  BEGIN
          GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
                              + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS +
                              GW_OUT_TOT-ZDJBYLBZ_GS + GW_OUT_TOT-YWBX_GS .
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  END

          "计税后工资总额 :/401 - 6150 - /403
          GW_OUT_TOT-JSHGZZE = GW_OUT_TOT-JSHGZZE + GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS - GW_OUT_TOT-GRSDS .
          "税后合计后总额 :/401-6150 - /403 + 7100 -(7020+7000+7030+7050+7010+7040+7080+7090+7060+7070）
          GW_OUT_TOT-SHHJZE =  GW_OUT_TOT-SHHJZE +  GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS -  GW_OUT_TOT-GRSDS + GW_OUT_TOT-SHQT  - ( GW_OUT_TOT-SSZSF + GW_OUT_TOT-SSSDF +
                           GW_OUT_TOT-GSFK + GW_OUT_TOT-JK + GW_OUT_TOT-GYJK + GW_OUT_TOT-CRMKK + GW_OUT_TOT-CFKJ + GW_OUT_TOT-SJQFKJ
                            + GW_OUT_TOT-LZGYF + GW_OUT_TOT-DNBZJ  ) .
          APPEND GW_OUT_TOT TO GT_OUT_TOT.
        ENDIF.
      ENDAT.
    ENDIF.

    IF P_G4 EQ 'X'.
      AT END OF C2_OBJ .
        IF GW_OUT_TOT IS NOT INITIAL.
          L_XH = L_XH   + 1 .
          GW_OUT_TOT-ZRS      = L_ZRS.
          GW_OUT_TOT-XH       = L_XH .
          GW_OUT_TOT-JSMON    = <LFS_OUT>-JSMON.
          GW_OUT_TOT-ABKRS    = <LFS_OUT>-ABKRS.
          "工资核算范围
          READ TABLE GT_T549T ASSIGNING <FS_T549T> WITH KEY ABKRS = GW_OUT_TOT-ABKRS BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GW_OUT_TOT-ATEXT         = <FS_T549T>-ATEXT.
          ENDIF.
          GW_OUT_TOT-A0_OBJ   = <LFS_OUT>-A0_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-A0_OBJ CHANGING  GW_OUT_TOT-A0  .
          GW_OUT_TOT-B0_OBJ   = <LFS_OUT>-B0_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-B0_OBJ CHANGING  GW_OUT_TOT-B0  .
          GW_OUT_TOT-C1_OBJ   = <LFS_OUT>-C1_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-C1_OBJ CHANGING  GW_OUT_TOT-C1  .
          GW_OUT_TOT-C2_OBJ   = <LFS_OUT>-C2_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-C2_OBJ CHANGING  GW_OUT_TOT-C2  .
*               "住房补助 :3061 + 3065
          GW_OUT_TOT-ZFBZ   = GW_OUT_TOT-ZFBZ + GW_OUT_TOT-ZFBZ01  + GW_OUT_TOT-ZFBZ02 .

          "当月绩效工资:1030 + 1031 ---黄芳增加
          GW_OUT_TOT-DYJXGZ    = GW_OUT_TOT-DYJXGZ + GW_OUT_TOT-JXGZBZ + GW_OUT_TOT-JXXS.
          "----------------------黄芳增加-----------------------------------


          "旷工扣款 ： 5575 + 5576
          GW_OUT_TOT-KGKK = GW_OUT_TOT-KGKK + GW_OUT_TOT-KGKK01 + GW_OUT_TOT-KGKK02.

          "应发工资总额 :/101 - 6160 - 6200
          GW_OUT_TOT-YFGZZE = GW_OUT_TOT-YFGZZE + ( GW_OUT_TOT-ZJE - GW_OUT_TOT-NZJJ - GW_OUT_TOT-LZBCJ  )  .

          "法定福利个人扣发合计 ：8313+8323+8333+8363+8373
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  BEGIN
*          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
*                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR.
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  END
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  BEGIN
          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR + GW_OUT_TOT-ZDJBYLBZ_GR + GW_OUT_TOT-SBTZ_GR +
                                     GW_OUT_TOT-GJJTZ_GR .
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  END

          "法定福利公司合计 ：8314 + 8324 + 8334 + 8344 + 8354 + 8364 + 8374
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  BEGIN
*         GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
*                            + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS.
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  END
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  BEGIN
          GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
                              + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS +
                              GW_OUT_TOT-ZDJBYLBZ_GS + GW_OUT_TOT-YWBX_GS .
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  END

          "计税后工资总额 :/401 - 6150 - /403
          GW_OUT_TOT-JSHGZZE = GW_OUT_TOT-JSHGZZE + GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS - GW_OUT_TOT-GRSDS .
          "税后合计后总额 :/401-6150 - /403 + 7100 -(7020+7000+7030+7050+7010+7040+7080+7090+7060+7070）
          GW_OUT_TOT-SHHJZE =  GW_OUT_TOT-SHHJZE +  GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS -  GW_OUT_TOT-GRSDS + GW_OUT_TOT-SHQT  - ( GW_OUT_TOT-SSZSF + GW_OUT_TOT-SSSDF +
                           GW_OUT_TOT-GSFK + GW_OUT_TOT-JK + GW_OUT_TOT-GYJK + GW_OUT_TOT-CRMKK + GW_OUT_TOT-CFKJ + GW_OUT_TOT-SJQFKJ
                            + GW_OUT_TOT-LZGYF + GW_OUT_TOT-DNBZJ  ) .
          APPEND GW_OUT_TOT TO GT_OUT_TOT.
        ENDIF.
      ENDAT.
    ENDIF.

    IF P_G5 EQ 'X'.
      AT END OF C3_OBJ .
        IF GW_OUT_TOT IS NOT INITIAL.
          L_XH = L_XH   + 1 .
          GW_OUT_TOT-ZRS      = L_ZRS.
          GW_OUT_TOT-XH       = L_XH .
          GW_OUT_TOT-JSMON    = <LFS_OUT>-JSMON.
          GW_OUT_TOT-ABKRS    = <LFS_OUT>-ABKRS.
          "工资核算范围
          READ TABLE GT_T549T ASSIGNING <FS_T549T> WITH KEY ABKRS = GW_OUT_TOT-ABKRS BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GW_OUT_TOT-ATEXT         = <FS_T549T>-ATEXT.
          ENDIF.
          GW_OUT_TOT-A0_OBJ   = <LFS_OUT>-A0_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-A0_OBJ CHANGING  GW_OUT_TOT-A0  .
          GW_OUT_TOT-B0_OBJ   = <LFS_OUT>-B0_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-B0_OBJ CHANGING  GW_OUT_TOT-B0  .
          GW_OUT_TOT-C1_OBJ   = <LFS_OUT>-C1_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-C1_OBJ CHANGING  GW_OUT_TOT-C1  .
          GW_OUT_TOT-C2_OBJ   = <LFS_OUT>-C2_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-C2_OBJ CHANGING  GW_OUT_TOT-C2  .
          GW_OUT_TOT-C3_OBJ   = <LFS_OUT>-C3_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-C3_OBJ CHANGING  GW_OUT_TOT-C3  .
*               "住房补助 :3061 + 3065
          GW_OUT_TOT-ZFBZ   = GW_OUT_TOT-ZFBZ + GW_OUT_TOT-ZFBZ01  + GW_OUT_TOT-ZFBZ02 .
          "当月绩效工资:1030 + 1031 ---黄芳增加
          GW_OUT_TOT-DYJXGZ    = GW_OUT_TOT-DYJXGZ + GW_OUT_TOT-JXGZBZ + GW_OUT_TOT-JXXS.
          "----------------------黄芳增加-----------------------------------


          "旷工扣款 ： 5575 + 5576
          GW_OUT_TOT-KGKK = GW_OUT_TOT-KGKK + GW_OUT_TOT-KGKK01 + GW_OUT_TOT-KGKK02.
          "应发工资总额 :/101 - 6160 - 6200

          GW_OUT_TOT-YFGZZE = GW_OUT_TOT-YFGZZE + ( GW_OUT_TOT-ZJE - GW_OUT_TOT-NZJJ - GW_OUT_TOT-LZBCJ  )  .

          "法定福利个人扣发合计 ：8313+8323+8333+8363+8373
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  BEGIN
*          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
*                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR.
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  END
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  BEGIN
          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR + GW_OUT_TOT-ZDJBYLBZ_GR + GW_OUT_TOT-SBTZ_GR +
                                     GW_OUT_TOT-GJJTZ_GR .
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  END

          "法定福利公司合计 ：8314 + 8324 + 8334 + 8344 + 8354 + 8364 + 8374
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  BEGIN
*         GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
*                            + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS.
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  END
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  BEGIN
          GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
                              + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS +
                              GW_OUT_TOT-ZDJBYLBZ_GS + GW_OUT_TOT-YWBX_GS .
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  END

          "计税后工资总额 :/401 - 6150 - /403
          GW_OUT_TOT-JSHGZZE = GW_OUT_TOT-JSHGZZE + GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS - GW_OUT_TOT-GRSDS .
          "税后合计后总额 :/401-6150 - /403 + 7100 -(7020+7000+7030+7050+7010+7040+7080+7090+7060+7070）
          GW_OUT_TOT-SHHJZE =  GW_OUT_TOT-SHHJZE +  GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS -  GW_OUT_TOT-GRSDS + GW_OUT_TOT-SHQT  - ( GW_OUT_TOT-SSZSF + GW_OUT_TOT-SSSDF +
                           GW_OUT_TOT-GSFK + GW_OUT_TOT-JK + GW_OUT_TOT-GYJK + GW_OUT_TOT-CRMKK + GW_OUT_TOT-CFKJ + GW_OUT_TOT-SJQFKJ
                            + GW_OUT_TOT-LZGYF + GW_OUT_TOT-DNBZJ  ) .
          APPEND GW_OUT_TOT TO GT_OUT_TOT.
        ENDIF.
      ENDAT.
    ENDIF.
    IF P_G6 EQ 'X'.
      AT END OF C4_OBJ .
        IF GW_OUT_TOT IS NOT INITIAL.
          L_XH = L_XH   + 1 .
          GW_OUT_TOT-ZRS      = L_ZRS.
          GW_OUT_TOT-XH       = L_XH .
          GW_OUT_TOT-JSMON    = <LFS_OUT>-JSMON.
          GW_OUT_TOT-ABKRS    = <LFS_OUT>-ABKRS.
          "工资核算范围
          READ TABLE GT_T549T ASSIGNING <FS_T549T> WITH KEY ABKRS = GW_OUT_TOT-ABKRS BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            GW_OUT_TOT-ATEXT         = <FS_T549T>-ATEXT.
          ENDIF.
          GW_OUT_TOT-A0_OBJ   = <LFS_OUT>-A0_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-A0_OBJ CHANGING  GW_OUT_TOT-A0  .
          GW_OUT_TOT-B0_OBJ   = <LFS_OUT>-B0_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-B0_OBJ CHANGING  GW_OUT_TOT-B0  .
          GW_OUT_TOT-C1_OBJ   = <LFS_OUT>-C1_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-C1_OBJ CHANGING  GW_OUT_TOT-C1  .
          GW_OUT_TOT-C2_OBJ   = <LFS_OUT>-C2_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-C2_OBJ CHANGING  GW_OUT_TOT-C2  .
          GW_OUT_TOT-C3_OBJ   = <LFS_OUT>-C3_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-C3_OBJ CHANGING  GW_OUT_TOT-C3  .
          GW_OUT_TOT-C4_OBJ   = <LFS_OUT>-C4_OBJ.
          PERFORM  SEL_ZZMS  USING GW_OUT_TOT-C4_OBJ CHANGING  GW_OUT_TOT-C4  .
*               "住房补助 :3061 + 3065
          GW_OUT_TOT-ZFBZ   = GW_OUT_TOT-ZFBZ + GW_OUT_TOT-ZFBZ01  + GW_OUT_TOT-ZFBZ02 .

          "当月绩效工资:1030 + 1031 ---黄芳增加
          GW_OUT_TOT-DYJXGZ    = GW_OUT_TOT-DYJXGZ + GW_OUT_TOT-JXGZBZ + GW_OUT_TOT-JXXS.
          "----------------------黄芳增加-----------------------------------

          "旷工扣款 ： 5575 + 5576
          GW_OUT_TOT-KGKK = GW_OUT_TOT-KGKK + GW_OUT_TOT-KGKK01 + GW_OUT_TOT-KGKK02.
          "应发工资总额 :/101 - 6160 - 6200

          GW_OUT_TOT-YFGZZE = GW_OUT_TOT-YFGZZE + ( GW_OUT_TOT-ZJE - GW_OUT_TOT-NZJJ - GW_OUT_TOT-LZBCJ  )  .

          "法定福利个人扣发合计 ：8313+8323+8333+8363+8373
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  BEGIN
*          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
*                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR.
*&--代码注释 BY HANDYBY 07.06.2017 14:16:36  END
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  BEGIN
          GW_OUT_TOT-FDFLKFHJ_GR = GW_OUT_TOT-FDFLKFHJ_GR + GW_OUT_TOT-YLAOBX_GR + GW_OUT_TOT-SYBX_GR + GW_OUT_TOT-YLIAOBX_GR
                                     + GW_OUT_TOT-GJJ_GR + GW_OUT_TOT-DBYLBX_GR + GW_OUT_TOT-ZDJBYLBZ_GR + GW_OUT_TOT-SBTZ_GR +
                                     GW_OUT_TOT-GJJTZ_GR .
*&--代码添加 BY HANDYBY 07.06.2017 14:16:47  END

          "法定福利公司合计 ：8314 + 8324 + 8334 + 8344 + 8354 + 8364 + 8374
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  BEGIN
*         GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
*                            + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS.
*&--代码注释 BY HANDYBY 07.06.2017 14:19:33  END
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  BEGIN
          GW_OUT_TOT-FDFLHJ_GS = GW_OUT_TOT-FDFLHJ_GS +  GW_OUT_TOT-YLAOBX_GS + GW_OUT_TOT-SYEBX_GS + GW_OUT_TOT-YLIAO_GS
                              + GW_OUT_TOT-GSBX_GS + GW_OUT_TOT-SYUBX_GS + GW_OUT_TOT-GJJ_GS + GW_OUT_TOT-DBYLBX_GS +
                              GW_OUT_TOT-ZDJBYLBZ_GS + GW_OUT_TOT-YWBX_GS .
*&--代码添加 BY HANDYBY 07.06.2017 14:19:47  END

          "计税后工资总额 :/401 - 6150 - /403
          GW_OUT_TOT-JSHGZZE = GW_OUT_TOT-JSHGZZE + GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS - GW_OUT_TOT-GRSDS .
          "税后合计后总额 :/401-6150 - /403 + 7100 -(7020+7000+7030+7050+7010+7040+7080+7090+7060+7070）
          GW_OUT_TOT-SHHJZE =  GW_OUT_TOT-SHHJZE +  GW_OUT_TOT-YSGZZE - GW_OUT_TOT-XJJS -  GW_OUT_TOT-GRSDS + GW_OUT_TOT-SHQT  - ( GW_OUT_TOT-SSZSF + GW_OUT_TOT-SSSDF +
                           GW_OUT_TOT-GSFK + GW_OUT_TOT-JK + GW_OUT_TOT-GYJK + GW_OUT_TOT-CRMKK + GW_OUT_TOT-CFKJ + GW_OUT_TOT-SJQFKJ
                            + GW_OUT_TOT-LZGYF + GW_OUT_TOT-DNBZJ  ) .
          APPEND GW_OUT_TOT TO GT_OUT_TOT.
        ENDIF.
      ENDAT.
    ENDIF.
  ENDLOOP.

  IF GT_OUT_TOT[] IS INITIAL.
    "MESSAGE S004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INITIAL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIAL_FIELDCAT .
  CLEAR: GT_FCAT[].


*  PERFORM FILL_FIELDCAT USING 'ABKRS' '工资核算范围代码' SPACE SPACE  2 SPACE SPACE SPACE.
*  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ATEXT' '工资核算范围' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JSMON' '薪资月份' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XH' '序号' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  IF P_KOSTL EQ 'X'.
    PERFORM FILL_FIELDCAT USING 'KOSTL' '成本中心' SPACE SPACE  30 SPACE SPACE SPACE.
    APPEND W_FCAT TO GT_FCAT.

    PERFORM FILL_FIELDCAT USING 'KOSTL_KTEXT' '成本中心描述' SPACE SPACE  30 SPACE SPACE SPACE.
    APPEND W_FCAT TO GT_FCAT.
  ELSE.
    PERFORM FILL_FIELDCAT USING 'A0' '集团' SPACE SPACE  30 SPACE SPACE SPACE.
    APPEND W_FCAT TO GT_FCAT.

    PERFORM FILL_FIELDCAT USING 'B0' '控股公司' SPACE SPACE  30 SPACE SPACE SPACE.
    APPEND W_FCAT TO GT_FCAT.


    PERFORM FILL_FIELDCAT USING 'C1' '一级部门' SPACE SPACE  30 SPACE SPACE SPACE.
    APPEND W_FCAT TO GT_FCAT.


    PERFORM FILL_FIELDCAT USING 'C2' '二级部门' SPACE SPACE  30 SPACE SPACE SPACE.
    APPEND W_FCAT TO GT_FCAT.


    PERFORM FILL_FIELDCAT USING 'C3' '三级部门' SPACE SPACE  30 SPACE SPACE SPACE.
    APPEND W_FCAT TO GT_FCAT.


    PERFORM FILL_FIELDCAT USING 'C4' '四级部门' SPACE SPACE  30 SPACE SPACE SPACE.
    APPEND W_FCAT TO GT_FCAT.

  ENDIF.

  PERFORM FILL_FIELDCAT USING 'ZRS' '计薪人数' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.


  PERFORM FILL_FIELDCAT USING 'DYJCGZ' '当月基础工资' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYGWGZ' '当月岗位工资' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYJXGZ' '当月绩效工资' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YWF' '业务费' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYBTGZ' '当月补贴工资' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GWJT' '岗位津贴' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXJT' '绩效津贴' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JNJT' '技能津贴' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 12.06.2017 10:20:08  BEGIN
  PERFORM FILL_FIELDCAT USING 'YGYGZ' '员工月工资' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 12.06.2017 10:20:08  END

  PERFORM FILL_FIELDCAT USING 'JJGZ' '计件工资' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JJBFJ' '计件补发金' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'TSGWBT' '特殊岗位补贴' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZFBZ' '住房补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CFBZ' '餐费补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GWBZ' '高温补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CCBZ' '出差补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'HFBZ' '话费补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 12.06.2017 10:20:08  BEGIN
  PERFORM FILL_FIELDCAT USING 'JTBZ' '交通补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BTZS' '补贴折算' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XLBT_JD' '学历补贴(金达)' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZYZGBT_JD' '执业资格补贴(金达)' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JSZCBT_JD' '技术职称补贴(金达)' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JTBT_JD' '交通补贴(金达)' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'HFBT_JD' '话费补贴(金达)' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CFBZ_JD' '餐费补助(金达)' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZFBT_JD' '住房补贴(金达)' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 12.06.2017 10:20:08  END

  PERFORM FILL_FIELDCAT USING 'GLJ' '工龄奖' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'QQJ' '全勤奖' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYXMBZ' '当月项目补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXKHZJ' '绩效考核增减' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JBBZ' '加班补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YBJT' '夜班津贴' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXGZYKJ' '绩效工资预扣减' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DNBT' '电脑补贴' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SSZBT' '宿舍长补贴' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GKBZ' '高空补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XSTC' '销售提成' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 12.06.2017 10:23:25  BEGIN
  PERFORM FILL_FIELDCAT USING 'BTZXHJ' '补贴杂项合计' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'WQBZ' '外勤补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'WPBZ' '外派补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XMBZ' '项目补助' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YJJJ' '业绩奖金' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'WCBT' '午餐补贴' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXJJ' '绩效奖金' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'QTKK' '其它扣款' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SHHFKK' '税后话费扣款' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 12.06.2017 10:23:25  END

  PERFORM FILL_FIELDCAT USING 'KZJJ' '考证奖金' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'QTJJ' '其他奖金' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'LBYPJE' '劳保用品金额' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JRFL' '节日福利' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'TSFLJ' '特殊福利金' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SQQT' '税前其他' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'PSJBF' '平时加班费' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZMJBF' '周末加班费' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'FDJJRJBF' '法定节假日加班费' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SJKK' '事假扣款' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BJKK' '病假扣款' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'KGKK' '旷工扣款' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YCKK' '迟到/早退/打卡异常扣款' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YFGZZE' '应发工资总额' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GJJ_GR' '公积金-个人' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YLAOBX_GR' '养老保险-个人' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SYBX_GR' '失业保险-个人' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YLIAOBX_GR' '医疗保险-个人' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DBYLBX_GR' '大病医疗保险-个人' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 12.06.2017 10:31:20  BEGIN
  PERFORM FILL_FIELDCAT USING 'ZDJBYLBZ_GR' '重大疾病医疗补助-个人' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SBTZ_GR' '社保调整-个人' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GJJTZ_GR' '公积金调整-个人' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 12.06.2017 10:31:20  END

  PERFORM FILL_FIELDCAT USING 'FDFLKFHJ_GR' '法定福利个人扣发合计' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GJJ_GS' '公积金-公司' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YLAOBX_GS' '养老保险-公司' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
  PERFORM FILL_FIELDCAT USING 'SYEBX_GS' '失业保险-公司' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YLIAO_GS' '医疗保险公司' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DBYLBX_GS' '大病医疗保险-公司' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GSBX_GS' '工伤保险-公司' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SYUBX_GS' '生育保险-公司' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 12.06.2017 10:32:39  BEGIN
  PERFORM FILL_FIELDCAT USING 'ZDJBYLBZ_GS' '重大疾病医疗补助-公司' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YWBX_GS' '意外保险-公司' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 12.06.2017 10:32:39  END

  PERFORM FILL_FIELDCAT USING 'FDFLHJ_GS' '法定福利公司合计' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XJJS' '现金计税' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YSGZZE' '应税工资总额' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GRSDS' '个人所得税' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JSHGZZE' '计税后工资总额' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SSZSF' '宿舍住宿费' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SSSDF' '宿舍水电费' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GSFK' '公司罚款' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JK' '借款' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GYJK' '公益捐款' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CRMKK' 'CRM扣款' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CFKJ' '餐费扣减' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SJQFKJ' '手机欠费扣减' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.


  PERFORM FILL_FIELDCAT USING 'LZGYF' '离职工衣费' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DNBZJ' '电脑保证金' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SHQT' '税后其他' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SHHJZE' '税后合计后总额' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.


  PERFORM FILL_FIELDCAT USING 'BFGZ' '补发工资' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'HFGZ' '缓发工资' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'NZJJ' '年终奖金' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'NZJS' '年终奖金税' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'LZBCJ' '离职补偿金' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'LZBCJS' '离职补偿金税' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYSFGZ' '当月实发工资 ' SPACE SPACE  30 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BYQK' '本月欠款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'QYQK' '前月欠款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 29.06.2017 20:08:01  BEGIN
  PERFORM FILL_FIELDCAT USING 'GLJT' '管理津贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZWJT' '职位津贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JSJT' '技术津贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'FJJBXZ' '附加基本薪资' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BTJT' '补贴津贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 29.06.2017 20:08:01  END


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INITIAL_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIAL_LAYOUT .
  CLEAR W_LAYOUT.
  W_LAYOUT-ZEBRA           = 'X'.
*  w_layout-stylefname      = 'CELLSTYL'.
  "  w_layout-info_fname   = 'ROWCOLOR'.
  W_LAYOUT-CWIDTH_OPT      = 'X'.
  W_LAYOUT-BOX_FNAME       = 'SEL'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER =
*     I_BUFFER_ACTIVE    =
      I_CALLBACK_PROGRAM = SY-REPID
*     i_callback_pf_status_set          = 'ALV_PF_STATUS'
*     i_callback_user_command           = 'ALV_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     i_grid_title       = l_title
*     I_GRID_SETTINGS    =
      IS_LAYOUT_LVC      = W_LAYOUT
      IT_FIELDCAT_LVC    = GT_FCAT
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS_LVC             =
*     IT_SORT_LVC        =
*     IT_FILTER_LVC      =
*     IT_HYPERLINK       =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      I_SAVE             = 'A'
      IS_VARIANT         = ALV_VARIANT
*     it_events          = gt_events
*     it_event_exit      = gt_event_exit
*     IS_PRINT_LVC       =
*     IS_REPREP_ID_LVC   =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  =
*     I_HTML_HEIGHT_END  =
*     IT_ALV_GRAPHICS    =
*     IT_EXCEPT_QINFO_LVC               =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_exit_CAUSED_BY_CALLER           =
*     ES_exit_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB           = GT_OUT_TOT
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM FILL_FIELDCAT USING FU_NAME
                         FU_TEXT
                         FU_TABLE
                         FU_FIELD
                         FU_OUTLEN
                         FU_KEY
                         FU_EDITMASK
                         FU_CHECKBOX.

  CLEAR W_FCAT.
  W_FCAT-FIELDNAME = FU_NAME.
  W_FCAT-COLTEXT   = FU_TEXT.
  W_FCAT-REF_TABLE = FU_TABLE.
  W_FCAT-REF_FIELD = FU_FIELD.
  W_FCAT-OUTPUTLEN = FU_OUTLEN.
  W_FCAT-KEY       = FU_KEY.
  W_FCAT-EDIT_MASK = FU_EDITMASK.
  W_FCAT-CHECKBOX  = FU_CHECKBOX.

ENDFORM.                    "fill_fieldcat
*&---------------------------------------------------------------------*
*&      Form  PROCESS_PAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LFS_PA0001>_PERNR  text
*----------------------------------------------------------------------*
FORM PROCESS_PAY_DATA  USING    FU_PERNR TYPE PA0001-PERNR..
  DATA: LT_RGDIR  TYPE STANDARD TABLE OF PC261,
        LT_PAYCN  TYPE STANDARD TABLE OF PAYCN_RESULT,      "工资核算结果 (中国)
        L_RELID   TYPE                   RELID_PCL,         "在表 PCLx 中簇的区域标识
        L_MOLGA   TYPE                   MOLGA,             "国家分组
        L_INDEX   TYPE                   SY-TABIX,          "索引，用于找最近调整前的工序序列处理差异
        L_SEQNR   TYPE                   PC261-SEQNR,
        L_SEQNR_C TYPE                   PC261-SEQNR,
        L_BEGDA   TYPE                   SY-DATUM,          "薪资月份的第一天
        L_ENDDA   TYPE                   SY-DATUM,          "薪资月份的最后一天
        L_ADD     TYPE                   C,                 "标识员工是否满足维度分析
        LW_OUT    TYPE                   TY_OUT.            "输出内表

  FIELD-SYMBOLS: <LFS_PC261>   TYPE PC261,
                 <LFS_PC261_C> TYPE PC261,                  "调整工资的序列
                 <LFS_PC261_B> TYPE PC261,                  "调整工资前的序列
                 <LFS_PAYCN>   TYPE PAYCN_RESULT.
  "取员工工资信息
  CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING
      PERSNR          = FU_PERNR
*     BUFFER          =
*     NO_AUTHORITY_CHECK       = ' '
*   IMPORTING
*     MOLGA           =
    TABLES
      IN_RGDIR        = LT_RGDIR
    EXCEPTIONS
      NO_RECORD_FOUND = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  "获取员工国家信息
  CALL FUNCTION 'PYXX_GET_RELID_FROM_PERNR'
    EXPORTING
      EMPLOYEE                    = FU_PERNR
    IMPORTING
      RELID                       = L_RELID
      MOLGA                       = L_MOLGA
    EXCEPTIONS
      ERROR_READING_INFOTYPE_0001 = 1
      ERROR_READING_MOLGA         = 2
      ERROR_READING_RELID         = 3
      OTHERS                      = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


  "根据工资月份和操作的月份升序排序
  SORT LT_RGDIR BY PAYTY FPPER INPER FPEND IPEND.

  "遍历当月工资发生的实际条目
  LOOP AT LT_RGDIR ASSIGNING <LFS_PC261>  WHERE PAYTY = P_PAYTY
                                           AND PAYID = P_PAYID.

    "对于工资计算年月为空，则根据日期处理
    IF <LFS_PC261>-INPER = '000000'..
      <LFS_PC261>-INPER = <LFS_PC261>-IPEND(6).
    ENDIF.

    IF <LFS_PC261>-FPPER = '000000'.
      <LFS_PC261>-FPPER = <LFS_PC261>-FPEND(6).
    ENDIF.
    "其中inper为操作调整工资的月份，fpper要调整工资的月份,当两个月份不相等时，认为调整工资的操作
    IF <LFS_PC261>-FPPER <> <LFS_PC261>-INPER.
      CONTINUE.
    ENDIF.

    "如果月份不在选择界面的期间则不进行后续处理，属于排除的数据
    IF <LFS_PC261>-FPPER NOT IN S_SPMON.
      CONTINUE.
    ENDIF.

    CLEAR: L_ENDDA,
           L_BEGDA,
           L_ADD.

    "获取计算薪资月份的最后一天
    CONCATENATE <LFS_PC261>-FPPER '01' INTO L_BEGDA.

    "获取结束日期
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = L_BEGDA
      IMPORTING
        LAST_DAY_OF_MONTH = L_ENDDA
      EXCEPTIONS
        DAY_IN_NO_DATE    = 1
        OTHERS            = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

    LW_OUT-PERNR          = FU_PERNR.
    "更新员工的其它信息
    PERFORM UPDATE_PERSON_INFOR USING LW_OUT
                                      L_ENDDA
                                      L_BEGDA
                                      L_ADD.
    "人员满足所选的维度分析
    IF L_ADD = SPACE.
      CONTINUE.
    ENDIF.

    "获取当前月工资信息
    PERFORM GET_PAY_DATA USING LW_OUT                "人员编号
                               L_RELID
                               <LFS_PC261>-SEQNR     "当前工资序列
                               SPACE
                               <LFS_PC261>-FPPER     "工资月份
                               <LFS_PC261>-FPPER.    "计算月份
    CLEAR: L_INDEX,
             L_SEQNR,
             L_SEQNR_C.

    "取当前月调整其它月的工资差异信息
    LOOP AT LT_RGDIR ASSIGNING <LFS_PC261_C> WHERE INPER = <LFS_PC261>-FPPER            "inper操作调整工资的月份
                                               AND FPPER < <LFS_PC261>-FPPER            "fpper要调整工资的月份
                                               AND PAYTY = <LFS_PC261>-PAYTY.
      "当前月调整的其它月的记录
      L_INDEX = SY-TABIX.

      "当前月调整的工资序列
      L_SEQNR = <LFS_PC261_C>-SEQNR.

      "当前月调整前的记录
      L_INDEX = L_INDEX - 1.

      "取当前月调整前的工资序列
      READ TABLE LT_RGDIR ASSIGNING <LFS_PC261_B> INDEX L_INDEX.
      IF SY-SUBRC EQ 0.
        "check 当前月调整前纪录的数据月和 当前月调整纪录的数据月是否相等
        IF <LFS_PC261_B>-FPPER EQ <LFS_PC261_C>-FPPER.
          L_SEQNR_C  = <LFS_PC261_B>-SEQNR.
        ELSE.
          CLEAR:L_SEQNR_C.
        ENDIF.
      ELSE.
        "   CONTINUE.
      ENDIF.

      "获取当前月调整其它月份的工资差异
      PERFORM GET_PAY_DATA USING LW_OUT
                                 L_RELID
                                 L_SEQNR                 "当前月调整的工资序列
                                 L_SEQNR_C               "当前月调整前的工资序列
                                 <LFS_PC261_C>-FPPER     "工资月份
                                 <LFS_PC261_C>-INPER.    "计算月份
    ENDLOOP.

    "清空数据
    CLEAR: LW_OUT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PERSON_INFOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_OUT  text
*      -->P_L_ENDDA  text
*      -->P_L_BEGDA  text
*      -->P_L_ADD  text
*----------------------------------------------------------------------*
FORM UPDATE_PERSON_INFOR  USING FU_OUT   TYPE TY_OUT
                                FU_ENDDA TYPE SY-DATUM
                                FU_BEGDA TYPE SY-DATUM
                                FU_ADD   TYPE C.
  DATA: L_DAR_FIELD TYPE STRING,
        L_DAT_FIELD TYPE STRING,
        L_NUM       TYPE N LENGTH 2,
        L_EXIST     TYPE C,
        L_STAT1     TYPE PA0000-STAT1,
        L_PLANS     TYPE PA0001-PLANS,
        L_ENDDA     TYPE SY-DATUM.

  FIELD-SYMBOLS: <LFS_DAR_FIELD>,
                 <LFS_DAT_FIELD>.
  SORT GT_T501T    BY PERSG.
  SORT GT_T503T    BY PERSK.
  "SORT GT_PA0185   BY PERNR SUBTY.
  SORT GT_T529U    BY STATV.
  SORT GT_HRP1000  BY OBJID.
  SORT GT_PA0041   BY PERNR.
  SORT GT_PA0000   BY PERNR.
  SORT GT_T500P    BY PERSA.
  SORT GT_T510G    BY MOLGA TRFGB.
  SORT GT_CSKT     BY KOSTL.
  LOOP AT GT_PA0001 ASSIGNING <FS_PA0001> WHERE PERNR = FU_OUT-PERNR
                                           AND BEGDA <= FU_ENDDA
                                           AND ENDDA >= FU_ENDDA.

    "权限检查
    PERFORM CHECK_AUTH USING <FS_PA0001>-VDSK1
                             FU_ADD.
    IF FU_ADD = SPACE.
      CONTINUE.
    ENDIF.

    FU_ADD = 'X'.

*    "工资核算范围
*    READ TABLE GT_T549T ASSIGNING <FS_T549T> WITH KEY ABKRS = <FS_PA0001>-ABKRS BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      FU_OUT-ATEXT         = <FS_T549T>-ATEXT.
*    ENDIF.

*    "支付等级范围
*    LOOP AT GT_PA0008 ASSIGNING <FS_PA0008> WHERE PERNR = <FS_PA0001>-PERNR
*                                               AND BEGDA <= FU_ENDDA
*                                               AND ENDDA >= FU_ENDDA.
*      FU_OUT-TRFGB         = <FS_PA0008>-TRFGB.
*      EXIT.
*    ENDLOOP.

    "工资核算范围
    FU_OUT-ABKRS            = <FS_PA0001>-ABKRS.

    "取成本中心
    IF P_KOSTL EQ 'X'.
      FU_OUT-KOSTL  = <FS_PA0001>-KOSTL.
*     READ TABLE  GT_CSKT ASSIGNING <FS_CSKT>   WITH KEY KOSTL = <FS_PA0001>-KOSTL BINARY SEARCH.
*     IF SY-SUBRC EQ 0 .
*       FU_OUT-KOSTL_KTEXT  = <FS_CSKT>-KTEXT .
*      ENDIF.

    ELSE.
      "取部门层级信息
      PERFORM GET_HIERARCHY_UP USING FU_OUT
                                     <FS_PA0001>-ORGEH
                                     FU_ADD.

      "根据选择维度清空不需要的维度信息
      PERFORM CLEAR_DATA USING FU_OUT.
    ENDIF.



    "退出处理
    EXIT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_PA0001>_VDSK1  text
*      -->P_FU_ADD  text
*----------------------------------------------------------------------*
FORM CHECK_AUTH  USING   FU_VDSK1 TYPE PA0001-VDSK1
                          FU_OK.

  FU_OK = 'X'.

  AUTHORITY-CHECK OBJECT 'P_ORGIN' ID 'VDSK1' FIELD FU_VDSK1.
  IF SY-SUBRC NE 0.
    FU_OK = SPACE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_HIERARCHY_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FU_OUT  text
*      -->P_<FS_PA0001>_ORGEH  text
*      -->P_FU_ADD  text
*----------------------------------------------------------------------*
FORM GET_HIERARCHY_UP  USING FU_OUT    TYPE TY_OUT
                             FU_ORGEH   TYPE PA0001-ORGEH
                             FU_ADD     TYPE C.

  DATA: L_CURR   TYPE HRP1001-OBJID,
        L_CURR_U TYPE HRP1001-OBJID,
        L_FIELD  TYPE STRING.

  FIELD-SYMBOLS: <LFS_FIELD>,
                 <LFS_HRP1001_L1> TYPE HRP1001.

  L_CURR = FU_ORGEH.

  WHILE L_CURR IS NOT INITIAL.

    "取组织结构的描述
    READ TABLE GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = L_CURR.
    IF SY-SUBRC EQ 0.

      L_FIELD = <FS_HRP1000>-SHORT.

      "如果
      IF L_FIELD =  G_TEXT.
        FU_ADD = 'X'.
      ELSE.

      ENDIF.

*      "更新组织结构描述
*      ASSIGN COMPONENT L_FIELD OF STRUCTURE FU_OUT TO <LFS_FIELD>.
*      IF SY-SUBRC EQ 0.
*
*        <LFS_FIELD> = <FS_HRP1000>-STEXT.
*
*      ENDIF.

      CONCATENATE L_FIELD C_OBJ_PRE INTO L_FIELD.

      "更新组织结构编号
      ASSIGN COMPONENT L_FIELD OF STRUCTURE FU_OUT TO <LFS_FIELD>.
      IF SY-SUBRC EQ 0.

        <LFS_FIELD> = <FS_HRP1000>-OBJID.

      ENDIF.

    ENDIF.

    "获取层级关系
    READ TABLE GT_HRP1001 ASSIGNING <FS_HRP1001> WITH KEY OBJID = L_CURR BINARY SEARCH.
    IF SY-SUBRC EQ 0.

      L_CURR = <FS_HRP1001>-SOBID.

    ELSE.

      CLEAR L_CURR.

    ENDIF.

  ENDWHILE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FU_OUT  text
*----------------------------------------------------------------------*
FORM CLEAR_DATA  USING   FU_OUT  TYPE TY_OUT.

  "对于选择集团，则清空控股、一、二、三、四级部门的值
  IF P_G1 = 'X'.
    CLEAR: FU_OUT-B0,
           FU_OUT-C1,
           FU_OUT-C2,
           FU_OUT-C3,
           FU_OUT-C4,
           FU_OUT-B0_OBJ,
           FU_OUT-C1_OBJ,
           FU_OUT-C2_OBJ,
           FU_OUT-C3_OBJ,
           FU_OUT-C4_OBJ.
  ENDIF.

  "对于选择控股，则清空一、二、三、四级部门的值
  IF P_G2 = 'X'.
    CLEAR: FU_OUT-C1,
           FU_OUT-C2,
           FU_OUT-C3,
           FU_OUT-C4,
           FU_OUT-C1_OBJ,
           FU_OUT-C2_OBJ,
           FU_OUT-C3_OBJ,
           FU_OUT-C4_OBJ.
  ENDIF.

  "对于选择-级部门，则清空二、三、四级部门的值
  IF P_G3 = 'X'.
    CLEAR: FU_OUT-C2,
           FU_OUT-C3,
           FU_OUT-C4,
           FU_OUT-C2_OBJ,
           FU_OUT-C3_OBJ,
           FU_OUT-C4_OBJ.
  ENDIF.

  "对于选择二部门，则清空三、四级部门的值
  IF P_G4 = 'X'.
    CLEAR: FU_OUT-C3,
           FU_OUT-C4,
           FU_OUT-C3_OBJ,
           FU_OUT-C4_OBJ.
  ENDIF.
  "对于选择三级部门，则清空四级部门的值
  IF P_G5 = 'X'.
    CLEAR:
           FU_OUT-C4,
           FU_OUT-C4_OBJ.
  ENDIF.
  "对于选择四级部门，则保留全部从集团到四级组织的组织信息

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_PAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_OUT  text
*      -->P_L_RELID  text
*      -->P_<LFS_PC261>_SEQNR  text
*      -->P_SPACE  text
*      -->P_<LFS_PC261>_FPPER  text
*      -->P_<LFS_PC261>_FPPER  text
*----------------------------------------------------------------------*
FORM  GET_PAY_DATA USING FU_OUT   TYPE TY_OUT
                         FU_RELID
                         FU_SEQNR                  "当前工序序列或者调整的工资序列
                         FU_SEQNR_C                "调整前的工资的序列
                         FU_GZMON                  "工资年月
                         FU_JSMON.                 "计算年月

  DATA: LW_PAYCN     TYPE                   PAYCN_RESULT,           "当月工资信息
        LW_PAYCN_C   TYPE                   PAYCN_RESULT,           "当前月调整工资信息
        LW_OUT_D     TYPE                   TY_OUT,                 "差异行
        L_INDEX      TYPE                   SY-TABIX,               "当前月工资索引
        L_FIELDNAME  TYPE                   STRING,                 "字段名
        L_VALUEFIELD TYPE                   STRING,                 "值的字段名
        L_LGART_VAL  TYPE                   STRING,                 "LGART的字段值
        L_VALUE      TYPE                   STRING,                 "字段值
        L_INTFIELD   TYPE                   STRING,                 "内表字段名
        L_PAY_D      TYPE                   MAXBT.                  "工资项差异
  DATA:  CY           TYPE                   C.

  FIELD-SYMBOLS: <LFS_RT>        TYPE LINE OF HRPAY99_RT,             "RT
                 <LFS_RT_C>      TYPE LINE OF HRPAY99_RT,
                 <LFS_TAX>       TYPE LINE OF HRPAYCN_TAX,            "TAX
                 <LFS_OUT>       TYPE         TY_OUT,
                 <LFS_FIELD>,
                 <LFS_INT_FIELD>,
                 <LFS_PAY>,
                 <LFS_PAY_C>.

  CLEAR: L_INDEX,
         LW_OUT_D,
         L_PAY_D.


  "取当前月工资信息
  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
    EXPORTING
      CLUSTERID                    = FU_RELID
      EMPLOYEENUMBER               = FU_OUT-PERNR
      SEQUENCENUMBER               = FU_SEQNR
*     READ_ONLY_BUFFER             = ' '
*     READ_ONLY_INTERNATIONAL      = ' '
*     ARC_GROUP                    = ' '
*     CHECK_READ_AUTHORITY         = 'X'
*     FILTER_CUMULATIONS           = 'X'
*     CLIENT                       =
*   IMPORTING
*     VERSION_NUMBER_PAYVN         =
*     VERSION_NUMBER_PCL2          =
    CHANGING
      PAYROLL_RESULT               = LW_PAYCN
    EXCEPTIONS
      ILLEGAL_ISOCODE_OR_CLUSTERID = 1
      ERROR_GENERATING_IMPORT      = 2
      IMPORT_MISMATCH_ERROR        = 3
      SUBPOOL_DIR_FULL             = 4
      NO_READ_AUTHORITY            = 5
      NO_RECORD_FOUND              = 6
      VERSIONS_DO_NOT_MATCH        = 7
      ERROR_READING_ARCHIVE        = 8
      ERROR_READING_RELID          = 9
      OTHERS                       = 10.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  "取本次调整工资前的工资信息，用于计算差异信息
  IF FU_SEQNR_C IS NOT INITIAL.

    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
      EXPORTING
        CLUSTERID                    = FU_RELID
        EMPLOYEENUMBER               = FU_OUT-PERNR
        SEQUENCENUMBER               = FU_SEQNR_C
*       READ_ONLY_BUFFER             = ' '
*       READ_ONLY_INTERNATIONAL      = ' '
*       ARC_GROUP                    = ' '
*       CHECK_READ_AUTHORITY         = 'X'
*       FILTER_CUMULATIONS           = 'X'
*       CLIENT                       =
*   IMPORTING
*       VERSION_NUMBER_PAYVN         =
*       VERSION_NUMBER_PCL2          =
      CHANGING
        PAYROLL_RESULT               = LW_PAYCN_C
      EXCEPTIONS
        ILLEGAL_ISOCODE_OR_CLUSTERID = 1
        ERROR_GENERATING_IMPORT      = 2
        IMPORT_MISMATCH_ERROR        = 3
        SUBPOOL_DIR_FULL             = 4
        NO_READ_AUTHORITY            = 5
        NO_RECORD_FOUND              = 6
        VERSIONS_DO_NOT_MATCH        = 7
        ERROR_READING_ARCHIVE        = 8
        ERROR_READING_RELID          = 9
        OTHERS                       = 10.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

  "当选择期间内时，需要将当月调整其它月的工资差异累计到当月
  " IF FU_SEQNR_C IS NOT INITIAL.
  .
  READ TABLE GT_OUT ASSIGNING <LFS_OUT> WITH KEY PERNR = FU_OUT-PERNR
                                                 GZMON = FU_JSMON
                                                 CYFLG = SPACE.              "取非差异工资行
  IF SY-SUBRC NE 0.
    "MESSAGE A005.
  ELSE.

    "定位当前月在输出内表中的索引
    CY = 'X'.
    L_INDEX = SY-TABIX.

  ENDIF.

  "  ENDIF.

  "统计工资信息(当月）
  LOOP AT LW_PAYCN-INTER-RT ASSIGNING <LFS_RT>.

    CLEAR: L_LGART_VAL,
           L_VALUEFIELD,
           L_INTFIELD.

    "LGART的字段值
    L_LGART_VAL  = <LFS_RT>-LGART.

    "计算工资项的值字段名
    L_VALUEFIELD = 'BETRG'.

    "根据工资项获取对应的处理字段名
    PERFORM GET_FIELDNAME USING <LFS_RT>-LGART      "工资项
                                L_VALUEFIELD        "值字段
                                L_INTFIELD.         "内表字段

    "获取当前工资项的值
    IF L_VALUEFIELD IS NOT INITIAL.
      ASSIGN COMPONENT L_VALUEFIELD OF STRUCTURE <LFS_RT> TO <LFS_PAY>.
      IF SY-SUBRC NE 0.
        "MESSAGE A007 WITH '<LFS_RT>' L_VALUEFIELD.
      ENDIF.
    ENDIF.

    "对应计算差异的情况
    IF   CY = 'X'.

      "调整前的工资项
      IF L_INTFIELD IS NOT INITIAL.

        READ TABLE LW_PAYCN_C-INTER-RT ASSIGNING <LFS_RT_C> WITH KEY LGART = L_LGART_VAL.
        IF SY-SUBRC EQ 0.

          "调整前的工资项的值
          ASSIGN COMPONENT L_VALUEFIELD OF STRUCTURE <LFS_RT_C> TO <LFS_PAY_C>.
          IF SY-SUBRC NE 0.
            "MESSAGE A007 WITH '<LFS_RT_C>' L_VALUEFIELD.
          ENDIF.

          "计算工资项差异 = 调整后工资 - 调整前工资
          L_PAY_D = <LFS_PAY> - <LFS_PAY_C>.

          "删除已处理的条目
          DELETE LW_PAYCN_C-INTER-RT INDEX SY-TABIX.

        ELSE.   "对应调整前不存在此工资项（因为取工资函数只取不等于0的工资项)

          "计算工资项差异 = 调整后工资
          L_PAY_D = <LFS_PAY>.

        ENDIF.

        IF L_PAY_D <> 0.

          "对应期间内，将差异累加到当月的工资
          IF P_QJN = 'X'.

            "其中<lfs_out>为当月的工资行
            ASSIGN COMPONENT L_INTFIELD OF STRUCTURE <LFS_OUT> TO <LFS_INT_FIELD>.
            IF SY-SUBRC EQ 0.

              "当月工资 = 当月工资 + 其它月在本月调整的工资差异
              <LFS_INT_FIELD> = <LFS_INT_FIELD> + L_PAY_D.

            ELSE.

              "MESSAGE A007 WITH '<LFS_OUT>' L_INTFIELD.

            ENDIF.

          ENDIF.

          "对应历经期，将差异体现在独立一行
          IF P_LJQ = 'X'.

            ASSIGN COMPONENT L_INTFIELD OF STRUCTURE LW_OUT_D TO <LFS_INT_FIELD>.
            IF SY-SUBRC EQ 0.

              "工资项差异
              <LFS_INT_FIELD> = L_PAY_D.

            ELSE.

              "MESSAGE A007 WITH 'LW_OUT_D' L_INTFIELD.

            ENDIF.

          ENDIF.

        ENDIF.


      ENDIF.
    ELSE.

      IF L_INTFIELD IS NOT INITIAL.

        "根据工资项更新对应的输出内表列，其中L_INTFIELD为内表工资项的字段名
        ASSIGN COMPONENT L_INTFIELD OF STRUCTURE FU_OUT TO <LFS_INT_FIELD>.
        IF SY-SUBRC EQ 0.
          <LFS_INT_FIELD>  = <LFS_PAY>.
        ELSE.
          "MESSAGE A007 WITH 'FU_OUT' L_INTFIELD.
        ENDIF.

********  读取税信息，只读取一条，理论上只有一条数据  *************
*        READ TABLE LW_PAYCN-NAT-TAX ASSIGNING <LFS_TAX> INDEX 1.
*        IF SY-SUBRC EQ 0.
*
*          "工资税率
*          ASSIGN COMPONENT 'GZSL' OF STRUCTURE FU_OUT TO <LFS_INT_FIELD>.
*          IF SY-SUBRC EQ 0.
*            <LFS_INT_FIELD> = <LFS_TAX>-TXPCT.
*          ENDIF.
*
*          "速算扣除数
*          ASSIGN COMPONENT 'SSKC' OF STRUCTURE FU_OUT TO <LFS_INT_FIELD>.
*          IF SY-SUBRC EQ 0.
*            <LFS_INT_FIELD> = <LFS_TAX>-TXACC.
*          ENDIF.
*
*          "免税额度
*          ASSIGN COMPONENT 'MSED' OF STRUCTURE FU_OUT TO <LFS_INT_FIELD>.
*          IF SY-SUBRC EQ 0.
*            <LFS_INT_FIELD> = <LFS_TAX>-EXPAM.
*          ENDIF.
*
*        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

  "对应差异的情况
  IF CY = 'X' .
    "其它剩余的差异行(对应调整前没有此工资项的情况）
    LOOP AT LW_PAYCN_C-INTER-RT ASSIGNING <LFS_RT_C>.

      CLEAR:L_VALUEFIELD,
            L_INTFIELD,
            L_PAY_D.

      "计算工资项的值字段名
      L_VALUEFIELD = 'BETRG'.

      "根据工资项获取对应的处理字段名
      PERFORM GET_FIELDNAME USING <LFS_RT_C>-LGART    "工资项
                                  L_VALUEFIELD        "值字段
                                  L_INTFIELD.         "内表字段

      IF L_INTFIELD IS INITIAL.
        CONTINUE.
      ENDIF.

      "调整前的工资项的值
      ASSIGN COMPONENT L_VALUEFIELD OF STRUCTURE <LFS_RT_C> TO <LFS_PAY_C>.
      IF SY-SUBRC NE 0.
        "MESSAGE A007 WITH '<LFS_RT_C>' L_VALUEFIELD.
      ENDIF.

      "计算工资项差异 = 0 - 调整前的工资项的值
      L_PAY_D = 0 - <LFS_PAY_C>.

      IF L_PAY_D <> 0.

        "对应期间内，将差异累加到当月的工资
        IF P_QJN = 'X'.

          "其中<lfs_out>为当月的工资行
          ASSIGN COMPONENT L_INTFIELD OF STRUCTURE <LFS_OUT> TO <LFS_INT_FIELD>.
          IF SY-SUBRC EQ 0.

            "当月工资 = 当月工资 + 其它月在本月调整的工资差异
            <LFS_INT_FIELD> = <LFS_INT_FIELD> + L_PAY_D.

          ELSE.

            "MESSAGE A007 WITH '<LFS_OUT>' L_INTFIELD.

          ENDIF.

        ENDIF.

        "对应历经期，将差异体现在独立一行
        IF P_LJQ = 'X'.

          ASSIGN COMPONENT L_INTFIELD OF STRUCTURE LW_OUT_D TO <LFS_INT_FIELD>.
          IF SY-SUBRC EQ 0.

            "工资项差异
            <LFS_INT_FIELD> = L_PAY_D.

          ELSE.

            "MESSAGE A007 WITH 'LW_OUT_D' L_INTFIELD.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.

    IF LW_OUT_D IS NOT INITIAL.

      "更新员工的其它信息
      LW_OUT_D-GZMON            = FU_GZMON.           "工资月份
      LW_OUT_D-JSMON            = FU_JSMON.           "计算月份
      LW_OUT_D-PERNR            = FU_OUT-PERNR.       "人员编号
      .
*      LW_OUT_D-SNAME            = FU_OUT-SNAME.       "姓名
*      LW_OUT_D-PERSG            = FU_OUT-PERSG.       "员工组
*      LW_OUT_D-PERSK            = FU_OUT-PERSK.       "员工子组
*      LW_OUT_D-GYZT             = FU_OUT-GYZT.        "雇佣状态
*      LW_OUT_D-RZDAT            = FU_OUT-RZDAT.       "入职日期
*      LW_OUT_D-ABKRS            = FU_OUT-ABKRS.       "工资核算范围代码
**      LW_OUT_D-ATEXT            = FU_OUT-ATEXT.       "工资核算范围文本
*      LW_OUT_D-ORGEH            = FU_OUT-ORGEH.       "部门
*      LW_OUT_D-PLANS            = FU_OUT-PLANS.       "岗位
*      LW_OUT_D-SFZH             = FU_OUT-SFZH.        "身份证号
      LW_OUT_D-CYFLG            = 'X'.                "标识差异行

      "插入到输出内表
      INSERT LW_OUT_D INTO GT_OUT INDEX L_INDEX.

    ENDIF.
  ELSE.

    FU_OUT-GZMON                = FU_GZMON.           "工资月份
    FU_OUT-JSMON                = FU_JSMON.           "计算月份

    .

    "添加到输出内表
    APPEND FU_OUT TO GT_OUT.

  ENDIF.

ENDFORM.                    " GET_PAY_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LFS_RT>_LGART  text
*      -->P_L_VALUEFIELD  text
*      -->P_L_INTFIELD  text
*----------------------------------------------------------------------*
FORM GET_FIELDNAME  USING L_LGART
                         L_VALUEFIELD
                         L_INTFIELD.
  CASE L_LGART .
      "当月基础工资
    WHEN '1010'.
      L_INTFIELD = 'DYJCGZ'.
      "当月岗位工资
    WHEN '1020'.
      L_INTFIELD = 'DYGWGZ'.
      "业务费
    WHEN '8010'.
      L_INTFIELD = 'YWF'.
      "当月补贴工资
    WHEN '1040'.
      L_INTFIELD = 'DYBTGZ'.
      "岗位津贴
    WHEN '2030'.
      L_INTFIELD = 'GWJT'.
      "绩效津贴
    WHEN '2010'.
      L_INTFIELD = 'JXJT'.
      "技能津贴
    WHEN '2020'.
      L_INTFIELD = 'JNJT'.
      "计件工资
    WHEN '4030'.
      L_INTFIELD = 'JJGZ'.
      "计件补发金
    WHEN '3090'.
      L_INTFIELD = 'JJBFJ'.
      "特殊岗位补贴
    WHEN '3320'.
      L_INTFIELD = 'TSGWBT'.
      "餐费补助
    WHEN '3100'.
      L_INTFIELD = 'CFBZ'.
      "高温补助
    WHEN '3020'.
      L_INTFIELD = 'GWBZ'.
      "出差补助
    WHEN '3080'.
      L_INTFIELD = 'CCBZ'.
      "话费补助
    WHEN '6010'.    "
      L_INTFIELD = 'HFBZ'.
      "工龄奖
    WHEN '3350'.
      L_INTFIELD = 'GLJ'.
      "全勤奖
    WHEN '3070'.
      L_INTFIELD = 'QQJ'.
      "当月项目补助
    WHEN '2113'.
      L_INTFIELD = 'DYXMBZ'.
      "绩效考核增减
    WHEN '3310'.
      L_INTFIELD = 'JXKHZJ'.
      "加班补助
    WHEN '3120'.
      L_INTFIELD = 'JBBZ'.
      "夜班津贴
    WHEN '3130'.
      L_INTFIELD = 'YBJT'.
      "绩效工资预扣减
    WHEN '3160'.
      L_INTFIELD = 'JXGZYKJ'.
      "电脑补贴
    WHEN '3170'.
      L_INTFIELD = 'DNBT'.
      "宿舍长补贴
    WHEN '3050'.
      L_INTFIELD = 'SSZBT'.
      "高空补助
    WHEN '3010'.
      L_INTFIELD = 'GKBZ'.
      "销售提成
    WHEN '3030'.
      L_INTFIELD = 'XSTC'.
      "考证奖金
    WHEN '6020'.
      L_INTFIELD = 'KZJJ'.
      "其他奖金
    WHEN '6130'.
      L_INTFIELD = 'QTJJ'.
      "劳保用品金额
    WHEN '3250'.
      L_INTFIELD = 'LBYPJE'.
      "节日福利
    WHEN '3280'.
      L_INTFIELD = 'JRFL'.
      "特殊福利金
    WHEN '3330'.
      L_INTFIELD = 'TSFLJ'.
      "税前其他
    WHEN '6060'.
      L_INTFIELD = 'SQQT'.
      "平时加班费
    WHEN '5515'.
      L_INTFIELD = 'PSJBF'.
      "周末加班费
    WHEN '5520'.
      L_INTFIELD = 'ZMJBF'.
      "法定节假日加班费
    WHEN '5530'.
      L_INTFIELD = 'FDJJRJBF'.

      "事假扣款
    WHEN '5131'.
      L_INTFIELD = 'SJKK'.
      "病假扣款
    WHEN '5161'.
      L_INTFIELD = 'BJKK'.
      "迟到/早退/打卡异常扣款
    WHEN '5110'.
      L_INTFIELD = 'YCKK'.
      "公积金-个人
    WHEN '8363'.
      L_INTFIELD = 'GJJ_GR'.
      "养老保险-个人
    WHEN '8313'.
      L_INTFIELD = 'YLAOBX_GR'.
      "失业保险-个人
    WHEN '8323'.
      L_INTFIELD = 'SYBX_GR'.
      "医疗保险-个人
    WHEN '8333'.
      L_INTFIELD = 'YLIAOBX_GR'.
      "大病医疗保险-个人
    WHEN '8373'.
      L_INTFIELD = 'DBYLBX_GR'.
      "公积金-公司
    WHEN '8364'.
      L_INTFIELD = 'GJJ_GS'.
      "养老保险-公司
    WHEN '8314'.
      L_INTFIELD = 'YLAOBX_GS'.
      "失业保险-公司
    WHEN '8324'.
      L_INTFIELD = 'SYEBX_GS'.
      "医疗保险公司
    WHEN '8334'.
      L_INTFIELD = 'YLIAO_GS'.
      "大病医疗保险-公司
    WHEN '8374'.
      L_INTFIELD = 'DBYLBX_GS'.
      "工伤保险-公司
    WHEN '8344'.
      L_INTFIELD = 'GSBX_GS'.
      "生育保险-公司
    WHEN '8354'.
      L_INTFIELD = 'SYUBX_GS'.
      "现金计税
    WHEN '6150'.
      L_INTFIELD = 'XJJS'.
      "应税工资总额
    WHEN '/401'.
      L_INTFIELD = 'YSGZZE'.
      "个人所得税
    WHEN '/403'.
      L_INTFIELD = 'GRSDS'.
      "宿舍住宿费
    WHEN '7020'.
      L_INTFIELD = 'SSZSF'.
      "宿舍水电费
    WHEN '7000'.
      L_INTFIELD = 'SSSDF'.
      "公司罚款
    WHEN '7030'.
      L_INTFIELD = 'GSFK'.
      "借款
    WHEN '7050'.
      L_INTFIELD = 'JK'.
      "公益捐款
    WHEN '7010'.
      L_INTFIELD = 'GYJK'.
      "CRM扣款
    WHEN '7040'.
      L_INTFIELD = 'CRMKK'.
      "餐费扣减
    WHEN '7080'.
      L_INTFIELD = 'CFKJ'.
      "手机欠费扣减
    WHEN '7090'.
      L_INTFIELD = 'SJQFKJ'.
      "离职工衣费
    WHEN '7060'.
      L_INTFIELD = 'LZGYF'.
      "电脑保证金
    WHEN '7070'.
      L_INTFIELD = 'DNBZJ'.
      "税后其他
    WHEN '7100'.
      L_INTFIELD = 'SHQT'.
      "补发工资
    WHEN '7120'.
      L_INTFIELD = 'BFGZ'.
      "缓发工资
    WHEN '7130'.
      L_INTFIELD = 'HFGZ'.
      "年终奖金
    WHEN '6160'.
      L_INTFIELD = 'NZJJ'.
      " 年终奖税
    WHEN '/404'.
      L_INTFIELD = 'NZJS'.
      "" 离职补偿金
    WHEN '6200'.
      L_INTFIELD = 'LZBCJ'.
      "离职补偿金税
    WHEN '/408'.
      L_INTFIELD = 'LZBCJS'.
      "当月实发工资
    WHEN '/560'.
      L_INTFIELD = 'DYSFGZ'.
      "绩效工资标准
    WHEN '1030'.
      L_INTFIELD = 'JXGZBZ'.
      "绩效系数
    WHEN '1031'.
      L_INTFIELD =  'JXXS'.
      "住房补助 （70小时内） 3061
    WHEN '3061'.
      L_INTFIELD = 'ZFBZ01'.
      "住房补贴(70及以上) 3065
    WHEN '3065'.
      L_INTFIELD =   'ZFBZ02' .
*      WHEN '3060'.
*         L_INTFIELD =   'ZFBZ' .
    WHEN '5575'.
      L_INTFIELD = 'KGKK01'.
    WHEN '5576'.
      L_INTFIELD = 'KGKK02'.
    WHEN '/101'.
      L_INTFIELD = 'ZJE'.
    WHEN '/561'.
      L_INTFIELD  = 'BYQK' ."本月欠款 /101
    WHEN '/563'.
      L_INTFIELD  = 'QYQK' ."前月欠款/101

*&--代码添加 BY HANDYBY 07.06.2017 13:58:43  BEGIN
    WHEN '1050'.
      L_INTFIELD  = 'YGYGZ' ."员工月工资
    WHEN '9003'.
      L_INTFIELD  = 'JTBZ' ."交通补助
    WHEN '9009'.
      L_INTFIELD  = 'BTZS' ."补贴折算
    WHEN '9001'.
      L_INTFIELD  = 'BTZXHJ' ."补贴杂项合计
    WHEN '9004'.
      L_INTFIELD  = 'WQBZ' ."外勤补助
    WHEN '9005'.
      L_INTFIELD  = 'WPBZ' ."外派补助
    WHEN '9006'.
      L_INTFIELD  = 'XMBZ' ."项目补助
    WHEN '9007'.
      L_INTFIELD  = 'YJJJ' ."业绩奖金
    WHEN '9012'.
      L_INTFIELD  = 'WCBT' ."午餐补贴
    WHEN '9008'.
      L_INTFIELD  = 'JXJJ' ."绩效奖金
    WHEN '9011'.
      L_INTFIELD  = 'QTKK' ."其他扣款
    WHEN '9013'.
      L_INTFIELD  = 'SHHFKK' ."税后话费扣款
    WHEN '9009'.
      L_INTFIELD  = 'BTZS' ."补贴折算
    WHEN '8383'.
      L_INTFIELD  = 'ZDJBYLBZ_GR' ."重大疾病医疗补助(个人)
    WHEN '8393'.
      L_INTFIELD  = 'SBTZ_GR' ."社保调整(个人)
    WHEN '8394'.
      L_INTFIELD  = 'GJJTZ_GR' ."公积金调整(个人)
    WHEN '8384'.
      L_INTFIELD  = 'ZDJBYLBZ_GS' ."重大疾病医疗补助(公司)
    WHEN '9014'.
      L_INTFIELD  = 'YWBX_GS' ."意外保险(公司)
*&--代码添加 BY HANDYBY 07.06.2017 13:58:43  END

*&--代码添加 BY HANDYBY 12.06.2017 10:08:00  BEGIN
    WHEN '9015'.
      L_INTFIELD  = 'XLBT_JD' ."学历补贴（金达）
    WHEN '9016'.
      L_INTFIELD  = 'ZYZGBT_JD' ."执业资格补贴（金达）
    WHEN '9017'.
      L_INTFIELD  = 'JSZCBT_JD' ."技术职称补贴（金达）
    WHEN '9018'.
      L_INTFIELD  = 'JTBT_JD' ."交通补贴（金达）
    WHEN '9019'.
      L_INTFIELD  = 'HFBT_JD' ."话费补贴（金达）
    WHEN '9020'.
      L_INTFIELD  = 'CFBT_JD' ."餐费补贴（金达）
    WHEN '9021'.
      L_INTFIELD  = 'ZFBT_JD' ."住房补贴（金达）
*&--代码添加 BY HANDYBY 12.06.2017 10:08:00  END

*&--代码添加 BY HANDYBY 29.06.2017 20:09:17  BEGIN
    WHEN '9022'.
      L_INTFIELD  = 'GLJT' ."管理津贴
    WHEN '9023'.
      L_INTFIELD  = 'ZWJT' ."职位津贴
    WHEN '9024'.
      L_INTFIELD  = 'JSJT' ."技术津贴
    WHEN '9025'.
      L_INTFIELD  = 'FJJBXZ' ."附加基本薪资
    WHEN '9026'.
      L_INTFIELD  = 'BTJT' ."补贴津贴
*&--代码添加 BY HANDYBY 29.06.2017 20:09:17  END

    WHEN OTHERS.
      CLEAR: L_VALUEFIELD,
             L_INTFIELD.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_ZZMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_OUT_TOT_A0_OBJ  text
*      <--P_GW_OUT_TOT_A0  text
*----------------------------------------------------------------------*
"取组织描述
FORM SEL_ZZMS  USING    P_OBJ
               CHANGING P_STEXT .
  READ TABLE GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID =  P_OBJ .
  IF SY-SUBRC EQ '0'.
    P_STEXT       = <FS_HRP1000>-STEXT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HROBJID_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HROBJID_HELP .
  DATA: PHROBJID TYPE STRING OCCURS 0 WITH HEADER LINE.

  REFRESH: S_ORGEH[].

  CALL FUNCTION 'RP_PNP_ORGSTRUCTURE'
    EXPORTING
      BEGDA  = SY-DATUM
      ENDDA  = SY-DATUM
      PLVAR  = '01'
    TABLES
      POBJID = PHROBJID[].

  LOOP AT PHROBJID.

    IF SY-TABIX = 1.
      CLEAR: S_ORGEH.
      S_ORGEH-SIGN   = PHROBJID(1).
      S_ORGEH-OPTION = PHROBJID+1(2).
      S_ORGEH-LOW    = PHROBJID+3(8).
      APPEND S_ORGEH.
    ENDIF.

    CLEAR S_ORGEH_SEL.
    S_ORGEH_SEL-SIGN   = PHROBJID(1).
    S_ORGEH_SEL-OPTION = PHROBJID+1(2).
    S_ORGEH_SEL-LOW    = PHROBJID+3(8).
    APPEND S_ORGEH_SEL.
    CLEAR GS_ZZCJ .
    GS_ZZCJ-ORGEH = S_ORGEH_SEL-LOW .
    "获取组织层级编码
    DATA:IM_OBJID TYPE OBJEKTID .
    IM_OBJID = GS_ZZCJ-ORGEH .
    CALL FUNCTION 'HR_HCP_READ_OBJECT_TEXT'
      EXPORTING
        IM_PLVAR = '01'
        IM_OTYPE = 'O'
*       IM_VIEW_HROBJID       =
*       IM_VIEW_KOKRS       =
        IM_OBJID = IM_OBJID
*       IM_ISTAT = ' '
        IM_BEGDA = SY-DATUM
        IM_ENDDA = SY-DATUM
      IMPORTING
        SHORT    = GS_ZZCJ-SHORT
*       LONG     =
      .
    APPEND GS_ZZCJ TO GT_ZZCJ .
    SORT GT_ZZCJ BY SHORT ORGEH  .

  ENDLOOP.
ENDFORM.
