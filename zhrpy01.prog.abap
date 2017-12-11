REPORT ZHRPY01.
*** tables & type-pools
TABLES: PERNR,
        MCS0,
        PA0008.
TYPES: BEGIN OF TY_OUT,
         GZMON         TYPE MCS0-SPMON,          "工资年月
         JSMON         TYPE MCS0-SPMON,          "计算年月
         ZZXH          TYPE N LENGTH 4 ,         "序号
         GZHSL         TYPE STRING,              "核算类别
         PAYID         TYPE PAYID,               "核算标识
         IPEND         TYPE IPEND,               "年终奖金核算日期
         PERNR         TYPE PERNR-PERNR,         "人员编号
         SNAME         TYPE EMNAM,                    " PERNR-SNAME,         "姓名
         OBJIDJT       TYPE ORGEH, "集团组织
         JTTEXT        TYPE STRING , "
         OBJIDKG       TYPE ORGEH, "控股组织
         KGTEXT        TYPE STRING , "控股组织名称
         ORGEH         TYPE ORGEH , "当前组织部门编号
         OBJIDC1       TYPE ORGEH, "一级部门
         C10RGEN       TYPE STRING, "一级部门名称
         OBJIDC2       TYPE ORGEH, "二级部门
         C20RGEN       TYPE STRING, "二级部门名称
         OBJIDC3       TYPE ORGEH, "三级部门
         C30RGEN       TYPE STRING, "三级部门名称
         OBJIDC4       TYPE ORGEH, "四级部门
         C40RGEN       TYPE STRING , "四级部门名称
         KOSTL         TYPE KOSTL, "成本中心
         KOSTL_KTEXT   TYPE KTEXT , "成本中心描述
         RZRQ          TYPE DATE, "当前司龄起算日期（入职日期）
         PLANS         TYPE PLANS, "岗位
         "PLANS_TXT    TYPE T528T-PLSTX ,"岗位描述
         PLANS_TXT     TYPE STRING,
         PERSG         TYPE PERSG, "员工组
         PERSG_PTEXT   TYPE T501T-PTEXT , "员工组名称
         PERSK         TYPE PERSK , "员工子组
         PERSK_PTEXT   TYPE T503T-PTEXT , "员工子组名称
         BUKRS         TYPE BUKRS, "公司代码
         BUKRS_BUTXT   TYPE T001-BUTXT, "公司代码名称
         WERKS         TYPE WERKS, "人事范围
         WERKS_NAME1   TYPE T500P-NAME1, "人事范围文本
         ABKRS         TYPE PERNR-ABKRS,         "工资核算范围编码
         ATEXT         TYPE STRING,              "工资核算范围文本
         TRFGB         TYPE TRFGB, "薪资等级区域
         TRFGB_TGBTX   TYPE T510G-TGBTX, "薪资等级区域文本
         ICNUM         TYPE PSG_IDNUM, "身份证号
         ICNUMZ1       TYPE PSG_IDNUM, "外籍护照号
         ZZGZYH        TYPE  ZDEGZYH, "工资卡银行名称
         ZZGZZH        TYPE ZDEGZZH , "工资卡号
*&--代码添加 BY HANDYBY 22.06.2017 10:13:02  BEGIN
         ZHMC          TYPE  ZDEZHMC,  "支行名称
*&--代码添加 BY HANDYBY 22.06.2017 10:13:02  END
*&--代码注释 BY HANDYBY 26.06.2017 11:03:15  BEGIN
*    YJXSS         TYPE KTAKL, "应应计薪时数
*         SJXSS         TYPE KTAKL, "实计薪时数
*&--代码注释 BY HANDYBY 26.06.2017 11:03:15  END
*&--代码添加 BY HANDYBY 26.06.2017 11:03:47  BEGIN
         YJXSS         TYPE MAXBT, "应应计薪时数
         SJXSS         TYPE MAXBT, "实计薪时数
*&--代码添加 BY HANDYBY 26.06.2017 11:03:47  END
         JCGZBZ        TYPE MAXBT, "基础工资标准
         DYJCGZ        TYPE MAXBT, "当月基础工资
         GWGZBZ        TYPE MAXBT, "岗位工资标准
         DYGWGZ        TYPE MAXBT, "当月岗位工资
         JXGZBZ        TYPE MAXBT, "绩效工资标准
*&--代码注释 BY HANDYBY 26.06.2017 11:04:07  BEGIN
*    JXGZXS_1      TYPE KTAKL , "绩效工资系统——数量
*&--代码注释 BY HANDYBY 26.06.2017 11:04:07  END
*&--代码添加 BY HANDYBY 26.06.2017 11:04:12  BEGIN
         JXGZXS_1      TYPE MAXBT , "绩效工资系统——数量
*&--代码添加 BY HANDYBY 26.06.2017 11:04:12  END
         JXGZXS        TYPE MAXBT, "绩效工资系数_金额
         DYJXGZ        TYPE MAXBT, "当月绩效工资
         YWF           TYPE MAXBT, "业务费
         BTGZBZ        TYPE MAXBT, "补贴工资标准
         DYBTGZ        TYPE MAXBT, "当月补贴工资
         GWJTBZ        TYPE MAXBT, "岗位津贴标准
         GWJT          TYPE MAXBT, "岗位津贴
         JXJTBZ        TYPE MAXBT, "绩效津贴标准
         JXJT          TYPE MAXBT, "绩效津贴
         JNJTBZ        TYPE MAXBT, "技能津贴标准
         JNJT          TYPE MAXBT, "技能津贴
*&--代码添加 BY HANDYBY 07.06.2017 10:23:07  BEGIN
         YGYGZ         TYPE MAXBT , "员工月工资
*&--代码添加 BY HANDYBY 07.06.2017 10:23:07  END
         JJDJ          TYPE MAXBT, "计件单价
*&--代码注释 BY HANDYBY 26.06.2017 11:04:27  BEGIN
*    JJGRGS        TYPE KTAKL, "计件个人工时
*&--代码注释 BY HANDYBY 26.06.2017 11:04:27  END
*&--代码添加 BY HANDYBY 26.06.2017 11:04:33  BEGIN
         JJGRGS        TYPE MAXBT, "计件个人工时
*&--代码添加 BY HANDYBY 26.06.2017 11:04:33  END
         JJGZ          TYPE MAXBT, "计件工资
         JJBFJ         TYPE MAXBT, "计件补发金
         TSGWBT        TYPE MAXBT, "特殊岗位补贴
         ZFBZ          TYPE MAXBT, "住房补助
         CFBZ          TYPE MAXBT, "餐费补助
         GWBZ          TYPE MAXBT, "高温补助
         CCBZ          TYPE MAXBT, "出差补助
         HFBZ          TYPE MAXBT, "话费补助
*&--代码添加 BY HANDYBY 07.06.2017 10:35:00  BEGIN
         JTBZ          TYPE MAXBT,  "交通补助
         BTZS          TYPE MAXBT,  "补贴折算
*&--代码添加 BY HANDYBY 07.06.2017 10:35:00  END
         GLJ           TYPE MAXBT, "工龄奖
         QQJ           TYPE MAXBT, "全勤奖
         XMBZYBZ       TYPE MAXBT, "项目补助月标准
         XMBZRBZ       TYPE MAXBT, "项目补助日标准
*&--代码注释 BY HANDYBY 26.06.2017 11:04:43  BEGIN
*    XMGZTS        TYPE KTAKL, "项目工作天数
*&--代码注释 BY HANDYBY 26.06.2017 11:04:43  END
*&--代码添加 BY HANDYBY 26.06.2017 11:04:49  BEGIN
         XMGZTS        TYPE MAXBT, "项目工作天数
*&--代码添加 BY HANDYBY 26.06.2017 11:04:49  END
         DYXMBZ        TYPE MAXBT, "当月项目补助
         JXKHZJ        TYPE MAXBT, "绩效考核增减
         JBBZ          TYPE MAXBT, "加班补助
*&--代码注释 BY HANDYBY 26.06.2017 11:04:58  BEGIN
*    YBS           TYPE KTAKL, "夜班数
*&--代码注释 BY HANDYBY 26.06.2017 11:04:58  END
*&--代码添加 BY HANDYBY 26.06.2017 11:05:05  BEGIN
         YBS           TYPE MAXBT, "夜班数
*&--代码添加 BY HANDYBY 26.06.2017 11:05:05  END
         YBJT          TYPE MAXBT, "夜班津贴
         JXGZYKJ       TYPE MAXBT, "绩效工资预扣减
         DNBT          TYPE MAXBT, "电脑补贴
         SSZBT         TYPE MAXBT, "宿舍长补贴
         GKBZ          TYPE MAXBT, "高空补助
         XSTC          TYPE MAXBT, "销售提成
*&--代码添加 BY HANDYBY 07.06.2017 10:36:13  BEGIN
         BTZXHJ        TYPE MAXBT,  "补贴杂项合计
         WQBZ          TYPE MAXBT,  "外勤补助
         WPBZ          TYPE MAXBT,  "外派补助
         XMBZ          TYPE MAXBT,  "项目补助
         YJJJ          TYPE MAXBT,  "业绩奖金
         WCBT          TYPE MAXBT,  "午餐补贴
         JXJJ          TYPE MAXBT,  "绩效奖金
         QTKK          TYPE MAXBT,  "其它扣款
         SHHFKK        TYPE MAXBT,  "税后话费扣款
*&--代码添加 BY HANDYBY 07.06.2017 10:36:13  END
*&--代码添加 BY HANDYBY 12.06.2017 09:36:53  BEGIN
         XLBT_JD       TYPE MAXBT, "学历补贴（金达）
         ZYZGBT_JD     TYPE MAXBT, "执业资格补贴（金达）
         JSZCBT_JD     TYPE MAXBT, "技术职称补贴（金达）
         JTBT_JD       TYPE MAXBT, "交通补贴（金达  ）
         HFBT_JD       TYPE MAXBT, "话费补贴（金达）
         CFBZ_JD       TYPE MAXBT, "餐费补助（金达）
         ZFBT_JD       TYPE MAXBT, "住房补贴（金达）
         BTHJ_JD       TYPE MAXBT, "补贴合计（金达）
         XLBTBZXS_JD   TYPE MAXBT, "学历补贴标准-显示（金达）
         ZYZGBTBZXS_JD TYPE MAXBT, "执业资格补贴标准-显示（金达)
         JSZCBTBZXS_JD TYPE MAXBT, "技术职称补贴标准-显示（金达）
         JTBTBZXS_JD   TYPE MAXBT, "交通补贴标准-显示（金达）
         HFBTXS_JD     TYPE MAXBT, "话费补贴-显示（金达）
         CFBZXS_JD     TYPE MAXBT, "餐费补助-显示（金达）
         ZFBTXS_JD     TYPE MAXBT, "住房补贴-显示（金达）
*&--代码添加 BY HANDYBY 12.06.2017 09:36:53  END
         KZJJ          TYPE MAXBT, "考证奖金
         QTJJ          TYPE MAXBT, "其他奖金
         LBYPJE        TYPE MAXBT, "劳保用品 金额
         JRFL          TYPE MAXBT, "节日福利
         TSFLJ         TYPE MAXBT, "特殊福利金
         SQQT          TYPE MAXBT, "税前其他

*&--代码注释 BY HANDYBY 23.06.2017 20:20:19  BEGIN
*    PSJBXSS       TYPE KTAKL, "平时加班小时数
*&--代码注释 BY HANDYBY 23.06.2017 20:20:19  END
*&--代码添加 BY HANDYBY 23.06.2017 20:20:09  BEGIN
         PSJBXSS       TYPE MAXBT, "平时加班小时数
*&--代码添加 BY HANDYBY 23.06.2017 20:20:09  END

         PSJBF         TYPE MAXBT, "平时加班费

*&--代码注释 BY HANDYBY 26.06.2017 11:05:19  BEGIN
*    ZMJBXSS       TYPE KTAKL, "周末加班小时数
*&--代码注释 BY HANDYBY 26.06.2017 11:05:19  END
*&--代码添加 BY HANDYBY 26.06.2017 11:05:25  BEGIN
         ZMJBXSS       TYPE MAXBT, "周末加班小时数
*&--代码添加 BY HANDYBY 26.06.2017 11:05:25  END

         ZMJBF         TYPE MAXBT, "周末加班费

*&--代码注释 BY HANDYBY 26.06.2017 11:05:19  BEGIN
*    FDJJRJBXS     TYPE KTAKL, "法定节假日加班小时数
*&--代码注释 BY HANDYBY 26.06.2017 11:05:19  END
*&--代码添加 BY HANDYBY 26.06.2017 11:05:25  BEGIN
         FDJJRJBXS     TYPE MAXBT, "法定节假日加班小时数
*&--代码添加 BY HANDYBY 26.06.2017 11:05:25  END

         FDJJRJBF      TYPE MAXBT, "法定节假日加班费

*&--代码注释 BY HANDYBY 26.06.2017 11:06:32  BEGIN
*    SJSS          TYPE KTAKL, "事假时数
*&--代码注释 BY HANDYBY 26.06.2017 11:06:32  END
*&--代码添加 BY HANDYBY 26.06.2017 11:06:38  BEGIN
         SJSS          TYPE MAXBT, "事假时数
*&--代码添加 BY HANDYBY 26.06.2017 11:06:38  END

         SJKK          TYPE MAXBT, "事假扣款
         BJSS          TYPE KTAKL , "病假实数
         BJKK          TYPE MAXBT, "病假扣款
         KGSS          TYPE KTAKL, "旷工时数
         KGKK          TYPE MAXBT, "旷工扣款
         CJSS          TYPE KTAKL, "产假时数
         CJKK          TYPE MAXBT, "产假扣款
         CZDAYC        TYPE MAXBT, "迟到/早退/打卡异常扣款
         YFGZ          TYPE MAXBT, "应发工资总额
         GJJ_GR        TYPE MAXBT, "公积金-个人
         YLAOBX_GR     TYPE MAXBT, "养老保险-个人
         SYBX_GR       TYPE MAXBT, "失业保险-个人
         YLBX_GR       TYPE MAXBT, "医疗保险-个人
         DBYLBX_GR     TYPE MAXBT, "大病医疗保险-个人
*&--代码添加 BY HANDYBY 07.06.2017 10:47:00  BEGIN
         ZDJBYLBZ_GR   TYPE MAXBT,  "重大疾病医疗补助（个人）
         SBTZ_GR       TYPE MAXBT,  "社保调整（个人）
         GJJTZ_GR      TYPE MAXBT,  "公积金调整（个人）
         SBHJ_GR       TYPE MAXBT,  "社保合计（个人）
*&--代码添加 BY HANDYBY 07.06.2017 10:47:00  END
         FDFL_GR       TYPE MAXBT, "法定福利个人扣发合计
         GJJ_GS        TYPE MAXBT, "公积金-公司
         YLAOBX_GS     TYPE MAXBT, "养老保险-公司
         SYBX_GS       TYPE MAXBT, "失业保险—公司
         YLBX_GS       TYPE MAXBT, "医疗保险--公司
         DBYLBX_GS     TYPE MAXBT, "大病医疗保险-公司
         GSBX_GS       TYPE MAXBT, "工伤保险（公司）
         SYUBX_GS      TYPE MAXBT, "生育保险（公司）
*&--代码添加 BY HANDYBY 07.06.2017 10:50:07  BEGIN
         ZDJBYLBZ_GS   TYPE MAXBT,  "重大疾病医疗补助（公司）
         YWBX_GS       TYPE MAXBT,  "意外保险（公司）
         SBHJ_GS       TYPE MAXBT,  "社保合计（公司）
*&--代码添加 BY HANDYBY 07.06.2017 10:50:07  END
         FDFL_GS       TYPE MAXBT, "法定福利公司
         XJJS          TYPE MAXBT, "现金计税
         YSGZZE        TYPE MAXBT, "应税工资总额
         EETYP         TYPE PCN_EETYP, "计税类型
         EETYP_TXT     TYPE STRING, "计税类型文本
         EXPAM         TYPE PCN_EXPAM, "免税额
         TXPCT         TYPE PTW_TXPCT, "税率
         TXACC         TYPE PTW_TXACC , "速算扣除数
         GRSDS         TYPE MAXBT , "个人所得税
         JSHGZZE       TYPE MAXBT, "计税后工资总额
         SSZSF         TYPE MAXBT, "宿舍住宿费
         SSSDF         TYPE MAXBT, "宿舍水电费
         GSFK          TYPE MAXBT, "公司罚款
         JK            TYPE MAXBT, "借款
         GYJK          TYPE MAXBT, "公益捐款
         CRMKK         TYPE MAXBT, "CRM扣款
         CFKK          TYPE MAXBT, "餐费扣款
         SJQFKK        TYPE MAXBT, "手机欠费扣款
         LZGYF         TYPE MAXBT, "离职工衣费
         DDBZJ         TYPE MAXBT, "电脑保证金
         SHQT          TYPE MAXBT, "税后其他
         SHHJHZE       TYPE MAXBT, "税后合计后总额
         BFGZ          TYPE MAXBT, "补发工资
         HFGZ          TYPE MAXBT, "缓发工资
         NZJJ          TYPE MAXBT, "年终奖金
         NZJS          TYPE MAXBT, "年终奖税
         LZBCJ         TYPE MAXBT, "离职补偿金
         LZBCJS        TYPE MAXBT, "离职补偿金税
         DYSFGZ        TYPE MAXBT, "当月实发工资
         ZDGZBZ        TYPE MAXBT, "最低工资标准

*&--代码添加 BY HANDYBY 07.06.2017 16:42:19  BEGIN
         DDTEXT        TYPE VAL_TEXT, "工资成本归属单位
*&--代码添加 BY HANDYBY 07.06.2017 16:42:19  END

         ZZBZ0         TYPE ZDEHRBZ, "工资备注1
         ZZBZ1         TYPE ZDEHRBZ, "工资备注2
         GZBZ          TYPE C LENGTH 200 , "工资备注
         JXGZBZ_1      TYPE MAXBT, " 1030：  绩效工资标准
         ZFBZ_1        TYPE MAXBT , "3061： 金额   住房补助（70小时内）
         ZFBT_1        TYPE MAXBT, "3065：金额 住房补贴（70及以上）

*&--代码注释 BY HANDYBY 26.06.2017 11:07:53  BEGIN
*    KGSS_1        TYPE KTAKL , "5175 : 数量  旷工时数（光电+金达）
*         KGSS_2        TYPE KTAKL, "5176: 数量  旷工时数（深圳LYD+电视）
*&--代码注释 BY HANDYBY 26.06.2017 11:07:53  END
*&--代码添加 BY HANDYBY 26.06.2017 11:08:00  BEGIN
         KGSS_1        TYPE MAXBT , "5175 : 数量  旷工时数（光电+金达）
         KGSS_2        TYPE MAXBT, "5176: 数量  旷工时数（深圳LYD+电视）
*&--代码添加 BY HANDYBY 26.06.2017 11:08:00  END

         KGKK_1        TYPE MAXBT, "5175 : 金额  旷工时数（光电+金达）
         KGKK_2        TYPE MAXBT, "5176 : 金额  旷工时数（光电+金达）
         ZJE_1         TYPE MAXBT, "总金额 /101
         BYQK          TYPE MAXBT, "本月欠款  /561
         QYQK          TYPE MAXBT, "前月欠款  /563

*&--代码添加 BY HANDYBY 29.06.2017 18:17:46  BEGIN
         GLJT          TYPE MAXBT,  "管理津贴
         ZWJT          TYPE MAXBT,  "职位津贴
         JSJT          TYPE MAXBT,  "技术津贴
         FJJBJT        TYPE MAXBT,  "附加基本薪资
         BTJT          TYPE MAXBT,  "补贴津贴
         GLJTBZ_XS     TYPE MAXBT,  "管理津贴标准-显示
         ZWJTBZ_XS     TYPE MAXBT,  "职位津贴标准-显示
         JSJTBZ_XS     TYPE MAXBT,  "技术津贴标准-显示
         FJJBXZBZ_XS   TYPE MAXBT,  "附加基本薪资标准-显示
         BTJTBZ_XS     TYPE MAXBT,  "补贴津贴标准-显示
*&--代码添加 BY HANDYBY 29.06.2017 18:17:46  END

***  非输出字段，只参与逻辑处理
         "TRFGB        TYPE PA0008-TRFGB,        "支付等级范围
         CYFLG         TYPE C,                   "差异
         NZJJBZ        TYPE STRING , "年终奖金备注
         YNSSDE        TYPE MAXBT, "应纳税所得额
         QQKC          TYPE MAXBT, "缺勤扣除数
         BTHJBZ_JD     TYPE MAXBT, "补贴标准合计（金达）
         SEL ,
       END   OF TY_OUT.
***  define internal table & work area
DATA: GT_OUT     TYPE STANDARD TABLE OF TY_OUT,
      GW_OUT     TYPE                   TY_OUT,
      GT_PA0001  TYPE STANDARD TABLE OF PA0001,
      GT_PA0000  TYPE STANDARD TABLE OF PA0000,
      GT_PA0041  TYPE STANDARD TABLE OF PA0041,
      GT_PA0185  TYPE STANDARD TABLE OF PA0185,
      GT_PA0008  TYPE STANDARD TABLE OF PA0008,
      GT_PA9128  TYPE STANDARD TABLE OF PA9128,

*&--代码添加 BY HANDYBY 07.06.2017 16:47:47  BEGIN
      GT_PA9029  TYPE STANDARD TABLE OF PA9029,
*&--代码添加 BY HANDYBY 07.06.2017 16:47:47  END

      GT_HRP1001 TYPE STANDARD TABLE OF HRP1001,
      GT_PA0009  TYPE STANDARD TABLE OF PA0009,

      "定义文本内表
      GT_T501T   TYPE STANDARD TABLE OF T501T,     "员工组文本
      GT_T503T   TYPE STANDARD TABLE OF T503T,     "员工子组文本
      GT_T527X   TYPE STANDARD TABLE OF T527X,     "组织单位文本
      GT_T529U   TYPE STANDARD TABLE OF T529U,     "雇佣状态文本
      GT_HRP1000 TYPE STANDARD TABLE OF HRP1000,   "组织岗位文本
      GT_T500P   TYPE STANDARD TABLE OF T500P,     "人事范围文本
      GT_T510G   TYPE STANDARD TABLE OF T510G,     "工资等级范围（工资体系)
      GT_T549T   TYPE STANDARD TABLE OF T549T,    "工资核算范围
      GT_T001    TYPE STANDARD TABLE OF T001 ,   "公司代码文本
      GT_CSKT    TYPE STANDARD TABLE OF CSKT,  "成本中心文本
      GT_PA0267  TYPE STANDARD TABLE OF PA0267. "附加非周期支付


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
      G_ENDDA TYPE SY-DATUM.                   "结束日期

CONSTANTS: C_T510G_MOLGA TYPE T510G-MOLGA VALUE '28'. "国家组

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
*&--代码添加 BY HANDYBY 07.06.2017 16:50:20  BEGIN
               <FS_PA9029>  TYPE PA9029,
*&--代码添加 BY HANDYBY 07.06.2017 16:50:20  END
               <FS_T500P>   TYPE T500P,
               <FS_PA0009>  TYPE PA0009,
               <FS_CSKT>    TYPE CSKT,
               <FS_T001>    TYPE T001,
               <FS_PA0267>  TYPE PA0267.


***  define selection-screen
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS S_SPMON FOR MCS0-SPMON NO-EXTENSION OBLIGATORY.            "薪资期间

PARAMETERS: P_QJN TYPE C RADIOBUTTON GROUP G2 DEFAULT 'X',            "期间内视图
            P_LJQ TYPE C RADIOBUTTON GROUP G2 .                       "历经期视图

SELECTION-SCREEN END   OF BLOCK BLK1.
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.

PARAMETERS: P_PAYTY TYPE PC261-PAYTY,                                   "工资核算类型
            P_PAYID TYPE PC261-PAYID.                                   "工资核算标识

SELECT-OPTIONS:  S_PERNR FOR PERNR-PERNR.                             "人员编号
SELECT-OPTIONS:
               S_BUKRS FOR PERNR-BUKRS NO-EXTENSION NO INTERVALS,                                 "公司代码
               S_WERKS FOR PERNR-WERKS NO-EXTENSION NO INTERVALS,                                 "人事范围
               S_TRFGB FOR PA0008-TRFGB NO-EXTENSION NO INTERVALS,"薪资等级区域
               S_ORGEH FOR PERNR-ORGEH NO-EXTENSION NO INTERVALS,       "部门
               S_ABKRS FOR PERNR-ABKRS NO-EXTENSION NO INTERVALS.                                "工资范围
***                 S_PAYID FOR PERNR-PAYID,                                 "人事范围
*                 S_ABKRS FOR PERNR-ABKRS,                                 "人事范围
*                 S_BUKRS FOR PERNR-BUKRS,                                 "公司代码
*                 S_JUPER FOR PERNR-JUPER,                                 "法人
*                 S_WERKS FOR PERNR-WERKS,                                 "人事范围
*                 S_BTRTL FOR PERNR-BTRTL,                                 "人事子范围
*                 S_KOSTL FOR PERNR-KOSTL,                                 "成本中心
*                 S_PERSG FOR PERNR-PERSG,                                 "员工组
*                 S_PERSK FOR PERNR-PERSK,                                 "员工子组


SELECTION-SCREEN END   OF BLOCK BLK2.

SELECTION-SCREEN: BEGIN OF BLOCK ANZ WITH FRAME TITLE TEXT-005.
PARAMETERS: ALV_DEF     LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN: END OF BLOCK ANZ.
*** INITIALIZATION
INITIALIZATION.

*** SCREEN PBO
AT SELECTION-SCREEN OUTPUT.
*  PERFORM PBO.

*** search help
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORGEH-LOW.
  PERFORM OBJID_HELP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR ALV_DEF.
  PERFORM ALV_F4.

*** AT SELECTION-SCREEN
AT SELECTION-SCREEN.

  PERFORM ALV_CHECK.

*** START-OF-SELECTION
START-OF-SELECTION.

  "检查数据
  " PERFORM CHECK_DATA.

  "获取人员数据
  PERFORM GET_PERSON_DATA.

  "处理数据
  PERFORM  PROCESS_DATA.

  "填充字段目录
  PERFORM INITIAL_FIELDCAT.

  "填充格式
  PERFORM INITIAL_LAYOUT.

  "调用ALV
  PERFORM DISPLAY_ALV.
*&---------------------------------------------------------------------*
*&      Form  OBJID_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OBJID_HELP .
  DATA: POBJID TYPE STRING OCCURS 0 WITH HEADER LINE.

  REFRESH: S_ORGEH[].

  CALL FUNCTION 'RP_PNP_ORGSTRUCTURE'
    EXPORTING
      BEGDA  = SY-DATUM
      ENDDA  = SY-DATUM
      PLVAR  = '01'
    TABLES
      POBJID = POBJID[].

  LOOP AT POBJID.

    IF SY-TABIX = 1.
      CLEAR: S_ORGEH.
      S_ORGEH-SIGN   = POBJID(1).
      S_ORGEH-OPTION = POBJID+1(2).
      S_ORGEH-LOW    = POBJID+3(8).
      APPEND S_ORGEH.
    ENDIF.

    CLEAR S_ORGEH_SEL.
    S_ORGEH_SEL-SIGN   = POBJID(1).
    S_ORGEH_SEL-OPTION = POBJID+1(2).
    S_ORGEH_SEL-LOW    = POBJID+3(8).
    APPEND S_ORGEH_SEL.

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
  ALV_VARIANT-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT = ALV_VARIANT
      I_SAVE     = 'A'
    IMPORTING
      ES_VARIANT = ALV_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    ALV_DEF = ALV_VARIANT-VARIANT.
  ENDIF.
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
  SET CURSOR                 FIELD  'ALV_DEF'.              "n725824

  ALV_VARIANT-REPORT = SY-REPID.
  ALV_VARIANT-VARIANT = ALV_DEF.

  IF ALV_DEF IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        I_SAVE     = 'A'
      CHANGING
        CS_VARIANT = ALV_VARIANT
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC <> 0.
      MESSAGE E321(M7) WITH ALV_DEF SY-REPID.
    ENDIF.
  ENDIF.
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

  "果部门条件不为空
  IF S_ORGEH[] IS NOT INITIAL AND S_ORGEH_SEL[] IS INITIAL.
    CLEAR S_ORGEH_SEL.
    S_ORGEH_SEL-SIGN   = S_ORGEH-SIGN.
    S_ORGEH_SEL-OPTION = S_ORGEH-OPTION.
    S_ORGEH_SEL-LOW    = S_ORGEH-LOW.
    APPEND S_ORGEH_SEL.
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
  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_PA0001
    FROM PA0001
    WHERE  PERNR IN S_PERNR "人员编号
      AND BUKRS IN  S_BUKRS "公司代码
      AND WERKS IN S_WERKS "人事范围
      AND ABKRS IN S_ABKRS  "工资范围
  AND ORGEH IN S_ORGEH_SEL ."组织编号
  IF  GT_PA0001[] IS NOT INITIAL.
    "取人员状态
    SELECT *
      INTO TABLE GT_PA0000
      FROM PA0000
      FOR ALL ENTRIES IN GT_PA0001
    WHERE PERNR = GT_PA0001-PERNR.
    "取薪资等级区域
    SELECT * INTO TABLE GT_CSKT
      FROM CSKT
      FOR  ALL ENTRIES IN GT_PA0001
    WHERE SPRAS = '1'  AND KOKRS = '1000' AND KOSTL = GT_PA0001-KOSTL.

    SORT  GT_CSKT BY KOKRS   KOSTL .
    "取 身份证号 、外籍 护照号
    SELECT * INTO TABLE GT_PA0009
      FROM PA0009
      FOR ALL ENTRIES IN GT_PA0001
    WHERE PERNR = GT_PA0001-PERNR.
    "取日期相关信息
    SELECT *
      INTO TABLE GT_PA0041
      FROM PA0041
      FOR ALL ENTRIES IN GT_PA0001
    WHERE PERNR = GT_PA0001-PERNR.

    "取身份证件信息
    SELECT *
      INTO TABLE GT_PA0185
      FROM PA0185
      FOR ALL ENTRIES IN GT_PA0001
    WHERE PERNR = GT_PA0001-PERNR.

    "取基本工资信息
    SELECT *
      INTO TABLE GT_PA0008
      FROM PA0008
      FOR ALL ENTRIES IN GT_PA0001
    WHERE PERNR = GT_PA0001-PERNR.

    "取备注文本信息
    SELECT *
      INTO TABLE GT_PA9128
      FROM PA9128
      FOR ALL ENTRIES IN GT_PA0001
    WHERE PERNR = GT_PA0001-PERNR.

*&--代码添加 BY HANDYBY 07.06.2017 16:48:49  BEGIN
    "取工资成本归属单位
    SELECT *
      INTO TABLE GT_PA9029
      FROM PA9029
      FOR ALL ENTRIES IN GT_PA0001
     WHERE PERNR = GT_PA0001-PERNR.
*&--代码添加 BY HANDYBY 07.06.2017 16:48:49  END

*  "取部门和岗位关系信息
*  SELECT *
*    INTO TABLE GT_HRP1001
*    FROM HRP1001
*  WHERE PLVAR = '01'                "计划版本
*    AND OTYPE IN ('O','S','P')      "对象类型
*    AND ISTAT = '1'                 "计划状态
*    AND RSIGN = 'A'                 "关系规格
*    AND RELAT = '002'.              "对象间的关系

    "取兼职岗位信息
    SELECT *
       INTO TABLE GT_HRP1001
      FROM HRP1001
      FOR ALL ENTRIES IN GT_PA0001
      WHERE PLVAR = '01'
      AND  OTYPE = 'P'
      AND  PROZT = 0
      AND  RSIGN = 'B'
      AND  RELAT = '008'
      AND  ISTAT = '1'
    AND  OBJID = GT_PA0001-PERNR.

***********  以下取文本信息     *******************************

    "员工组文本
    SELECT *
      INTO TABLE GT_T501T
      FROM T501T
    WHERE  SPRSL = '1'.

    "员工子组
    SELECT *
      INTO TABLE GT_T503T
      FROM T503T
    WHERE SPRSL = '1'.

    "取组织岗位文本
    SELECT *
      INTO TABLE GT_HRP1000
      FROM HRP1000
    WHERE PLVAR = '01'                "计划版本
      AND OTYPE IN ('O','S' )",'P')      "对象类型
    AND ISTAT = '1'.                "计划状态

    "取雇佣状态
    SELECT *
      INTO TABLE GT_T529U
      FROM T529U
    WHERE SPRSL = SY-LANGU
    AND STATN = '1'.

    "取工资核算范围的文本
    SELECT *
      INTO TABLE GT_T549T
      FROM T549T
    WHERE SPRSL = '1'.

    "取人事范围文本
    SELECT *
      INTO TABLE GT_T500P
      FROM T500P
    WHERE PERSA IN S_WERKS.

    "取工资体系描述
    SELECT *
      INTO TABLE GT_T510G
    FROM T510G.

    "取公司代码 数据
    SELECT * INTO TABLE GT_T001 FROM  T001
    FOR ALL ENTRIES IN GT_PA0001
    WHERE BUKRS = GT_PA0001-BUKRS..

    "取附加非周期支付
    SELECT * INTO TABLE GT_PA0267 FROM PA0267 .


***********  以上取文本信息     *******************************
  ENDIF.
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
  FIELD-SYMBOLS: <LFS_PA0001> TYPE PA0001,
                 <LFS_OUT>    TYPE TY_OUT.

  SORT GT_PA0001   BY PERNR.

  "遍历人员数据
  LOOP AT GT_PA0001 ASSIGNING <LFS_PA0001>.

    AT NEW PERNR.

************ 调用子例程获取工资信息   *********************

      PERFORM PROCESS_PAY_DATA USING <LFS_PA0001>-PERNR.

************ 调用子例程获取工资信息   *********************
    ENDAT.

  ENDLOOP.

  LOOP AT GT_OUT   ASSIGNING <LFS_OUT> .
    "序号赋值
    <LFS_OUT>-ZZXH = SY-TABIX.
    "当月绩效工资 1030+1031
    <LFS_OUT>-DYJXGZ = <LFS_OUT>-JXGZBZ_1 + <LFS_OUT>-JXGZXS .
    "住房补助 3061+3065
    <LFS_OUT>-ZFBZ = <LFS_OUT>-ZFBZ_1 + <LFS_OUT>-ZFBT_1 .
    " 旷工时数  取RT表中5175+5176工资项所对应的数量
    <LFS_OUT>-KGSS = <LFS_OUT>-KGSS_1 + <LFS_OUT>-KGSS_2 .
    "旷工扣款  取RT表中5575+5576工资项所对应的金额
    <LFS_OUT>-KGKK = <LFS_OUT>-KGKK_1 + <LFS_OUT>-KGKK_2.
    "应发工资总额  /101-6160-6200  金额
    <LFS_OUT>-YFGZ = <LFS_OUT>-ZJE_1 - <LFS_OUT>-NZJJ - <LFS_OUT>-LZBCJ .

    "  法定福利个人扣发合计 8313+8323+833+8363+8373 金额
*&--代码注释 BY HANDYBY 07.06.2017 13:47:36  BEGIN
*    <LFS_OUT>-FDFL_GR = <LFS_OUT>-YLAOBX_GR + <LFS_OUT>-SYBX_GR + <LFS_OUT>-YLBX_GR + <LFS_OUT>-GJJ_GR + <LFS_OUT>-DBYLBX_GR.
*&--代码注释 BY HANDYBY 07.06.2017 13:47:36  END
*&--代码添加 BY HANDYBY 07.06.2017 13:47:24  BEGIN
    <LFS_OUT>-FDFL_GR = <LFS_OUT>-YLAOBX_GR + <LFS_OUT>-SYBX_GR + <LFS_OUT>-YLBX_GR + <LFS_OUT>-GJJ_GR + <LFS_OUT>-DBYLBX_GR +
                        <LFS_OUT>-ZDJBYLBZ_GR + <LFS_OUT>-SBTZ_GR + <LFS_OUT>-GJJTZ_GR .
*&--代码添加 BY HANDYBY 07.06.2017 13:47:24  END

    " 法定福利公司合计 8314+8324+8334+8344+8354+8364+8374 金额
*&--代码注释 BY HANDYBY 07.06.2017 13:49:06  BEGIN
*    <LFS_OUT>-FDFL_GS = <LFS_OUT>-YLAOBX_GS + <LFS_OUT>-SYBX_GS + <LFS_OUT>-YLBX_GS + <LFS_OUT>-GSBX_GS + <LFS_OUT>-SYUBX_GS + <LFS_OUT>-GJJ_GS + <LFS_OUT>-DBYLBX_GS .
*&--代码注释 BY HANDYBY 07.06.2017 13:49:06  END
*&--代码添加 BY HANDYBY 07.06.2017 13:49:23  BEGIN
    <LFS_OUT>-FDFL_GS = <LFS_OUT>-YLAOBX_GS + <LFS_OUT>-SYBX_GS + <LFS_OUT>-YLBX_GS + <LFS_OUT>-GSBX_GS + <LFS_OUT>-SYUBX_GS +
                        <LFS_OUT>-GJJ_GS + <LFS_OUT>-DBYLBX_GS + <LFS_OUT>-ZDJBYLBZ_GS + <LFS_OUT>-YWBX_GS .
*&--代码添加 BY HANDYBY 07.06.2017 13:49:23  END

*&--代码添加 BY HANDYBY 15.06.2017 14:52:33  BEGIN
    <LFS_OUT>-SBHJ_GR = <LFS_OUT>-YLAOBX_GR + <LFS_OUT>-SYBX_GR + <LFS_OUT>-YLBX_GR + <LFS_OUT>-DBYLBX_GR + <LFS_OUT>-ZDJBYLBZ_GR +
                        <LFS_OUT>-SBTZ_GR .
    <LFS_OUT>-SBHJ_GS = <LFS_OUT>-YLAOBX_GS + <LFS_OUT>-SYBX_GS + <LFS_OUT>-YLBX_GS + <LFS_OUT>-GSBX_GS + <LFS_OUT>-SYUBX_GS +
                        <LFS_OUT>-DBYLBX_GS + <LFS_OUT>-ZDJBYLBZ_GS + <LFS_OUT>-YWBX_GS .
*&--代码添加 BY HANDYBY 15.06.2017 14:52:33  END

    "计税后工资总额  /401-6150-/403
    <LFS_OUT>-JSHGZZE =  <LFS_OUT>-YSGZZE - <LFS_OUT>-XJJS - <LFS_OUT>-GRSDS .
    " 税后合计后总额   /401-6150-/403 +7100  -(7020+7000+7030+7050+7010+7040+7080+7090+7060+7070)
    <LFS_OUT>-SHHJHZE = <LFS_OUT>-YSGZZE - <LFS_OUT>-XJJS - <LFS_OUT>-GRSDS  + <LFS_OUT>-SHQT  - ( <LFS_OUT>-SSZSF + <LFS_OUT>-SSSDF +
                                <LFS_OUT>-GSFK + <LFS_OUT>-JK + <LFS_OUT>-GYJK + <LFS_OUT>-CRMKK +
                                <LFS_OUT>-CFKK + <LFS_OUT>-SJQFKK + <LFS_OUT>-LZGYF + <LFS_OUT>-DDBZJ ) .
    "应纳税所得额
    <LFS_OUT>-YNSSDE = <LFS_OUT>-YSGZZE  - <LFS_OUT>-EXPAM .

*&--代码添加 BY HANDYBY 12.06.2017 09:56:45  BEGIN
    "补贴合计（金达）
    <LFS_OUT>-BTHJ_JD = <LFS_OUT>-XLBT_JD + <LFS_OUT>-ZYZGBT_JD + <LFS_OUT>-JSZCBT_JD + <LFS_OUT>-JTBT_JD + <LFS_OUT>-HFBT_JD +
                        <LFS_OUT>-CFBZ_JD + <LFS_OUT>-ZFBT_JD .
*&--代码添加 BY HANDYBY 12.06.2017 09:56:45  END
*&--代码添加 BY IT02 20171101 BEGIN
    "补贴合计标准(金达)
    <LFS_OUT>-BTHJBZ_JD = <LFS_OUT>-XLBTBZXS_JD + <LFS_OUT>-ZYZGBTBZXS_JD + <LFS_OUT>-JSZCBTBZXS_JD + <LFS_OUT>-JTBTBZXS_JD
                        + <LFS_OUT>-HFBTXS_JD + <LFS_OUT>-CFBZXS_JD + <LFS_OUT>-ZFBTXS_JD.
*&--代码添加 BY IT02 20171101 END
*&--代码添加 BY IT02 20171025 BEGIN
    "缺勤扣除
    <LFS_OUT>-QQKC = <LFS_OUT>-JCGZBZ - <LFS_OUT>-DYJCGZ + <LFS_OUT>-GWGZBZ - <LFS_OUT>-DYGWGZ
                   + <LFS_OUT>-JXGZBZ - <LFS_OUT>-DYJXGZ + <LFS_OUT>-BTGZBZ - <LFS_OUT>-DYBTGZ.
                   "+ <LFS_OUT>-BTHJBZ_JD - <LFS_OUT>-BTHJ_JD.
*&--代码添加 BY IT02 20171025 END

  ENDLOOP.
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

  PERFORM FILL_FIELDCAT USING 'JSMON' '工资年月' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GZMON' '数据年月' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZZXH' '序号' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GZHSL' '核算类型' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'PAYID' '核算标识' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  IF P_PAYTY = 'A'.
    PERFORM FILL_FIELDCAT USING 'IPEND' '年终奖金核算日期' SPACE SPACE  8 SPACE SPACE SPACE.
    APPEND W_FCAT TO GT_FCAT.
  ENDIF.

  PERFORM FILL_FIELDCAT USING 'PERNR' '工号' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SNAME' '姓名' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  "  PERFORM FILL_FIELDCAT USING 'SNAME' '姓名' SPACE SPACE  8 SPACE SPACE SPACE.
  " APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JTTEXT' '集团' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'KGTEXT' '控股公司' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'C10RGEN' '一级部门名称' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'C20RGEN' '二级部门名称' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'C30RGEN' '三级部门名称' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'C40RGEN' '四级部门名称' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'KOSTL' '成本中心编码' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'KOSTL_KTEXT' '成本中心描述' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'RZRQ' '当前司龄起算日期（入职日期)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING  'PLANS_TXT' '职位' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'PERSG_PTEXT' '员工组' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'PERSK_PTEXT' '员工子组' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BUKRS_BUTXT' '公司名称' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'WERKS_NAME1' '人事范围' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'TRFGB_TGBTX' '薪资等级区域' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ICNUM ' '身份证号' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ICNUMZ1' '外籍护照号' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZZGZYH' '工资卡银行名称' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZZGZZH ' '工资卡号' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 22.06.2017 10:14:57  BEGIN
  PERFORM FILL_FIELDCAT USING 'ZHMC ' '支行名称' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 22.06.2017 10:14:57  END

  PERFORM FILL_FIELDCAT USING 'YJXSS' '应计薪时数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SJXSS' '实计薪时数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JCGZBZ' '基础工资标准' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYJCGZ' '当月基础工资' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GWGZBZ' '岗位工资标准' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYGWGZ' '当月岗位工资' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXGZBZ' '绩效工资标准' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXGZXS_1' '绩效工资系数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYJXGZ' '当月绩效工资' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YWF' '业务费' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BTGZBZ' '补贴工资标准' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYBTGZ' '当月补贴工资' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GWJTBZ' '岗位津贴标准' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GWJT' '岗位津贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXJTBZ' '绩效津贴标准' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXJT' '绩效津贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JNJTBZ' '技能津贴标准' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JNJT' '技能津贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 07.06.2017 13:19:43  BEGIN
  PERFORM FILL_FIELDCAT USING 'YGYGZ' '员工月工资' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 07.06.2017 13:19:43  END

  PERFORM FILL_FIELDCAT USING 'JJDJ' '计件单价' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JJGRGS' '计件个人工时' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JJGZ' '计件工资' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JJBFJ' '计件补发金' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'TSGWBT' '特殊岗位补贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZFBZ' '住房补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CFBZ' '餐费补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GWBZ' '高温补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CCBZ' '出差补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'HFBZ' '话费补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 07.06.2017 13:19:43  BEGIN
  PERFORM FILL_FIELDCAT USING 'JTBZ' '交通补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BTZS' '补贴折算' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 07.06.2017 13:19:43  END

  PERFORM FILL_FIELDCAT USING 'GLJ' '工龄奖' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'QQJ' '全勤奖' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XMBZYBZ' '项目补助月标准' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XMBZRBZ' '项目补助日标准' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XMGZTS' '项目工作天数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYXMBZ' '当月项目补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXKHZJ' '绩效考核增减' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JBBZ' '加班补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YBS' '夜班次数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YBJT' '夜班津贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXGZYKJ' '绩效工资预扣减' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DNBT' '电脑补贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SSZBT' '宿舍长补贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GKBZ' '高空补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XSTC' '销售提成' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 07.06.2017 13:19:43  BEGIN
  PERFORM FILL_FIELDCAT USING 'BTZXHJ' '补贴杂项合计' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'WQBZ' '外勤补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'WPBZ' '外派补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XMBZ' '项目补助' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YJJJ' '业绩奖金' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'WCBT' '午餐补贴' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JXJJ' '绩效奖金' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'QTKK' '其他扣款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SHHFKK' '税后话费扣款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 07.06.2017 13:19:43  END

*&--代码添加 BY HANDYBY 12.06.2017 09:47:19  BEGIN
  PERFORM FILL_FIELDCAT USING 'XLBT_JD' '学历补贴(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZYZGBT_JD' '执业资格补贴(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JSZCBT_JD' '技术职称补贴(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JTBT_JD' '交通补贴(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'HFBT_JD' '通讯补助(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CFBZ_JD' '餐费补助(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZFBT_JD' '住房补贴(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BTHJ_JD' '补贴合计(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XLBTBZXS_JD' '学历补贴标准(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZYZGBTBZXS_JD' '执业资格补贴标准(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JSZCBTBZXS_JD' '技术职称补贴标准(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JTBTBZXS_JD' '交通补贴标准(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'HFBTXS_JD' '通讯补贴标准(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CFBZXS_JD' '餐费补助标准(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZFBTXS_JD' '住房补贴标准(金达)' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 12.06.2017 09:47:19  END

  PERFORM FILL_FIELDCAT USING 'KZJJ' '考证奖金' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'QTJJ' '其他奖金' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'LBYPJE' '劳保用品金额' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JRFL' '节日福利' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'TSFLJ' '特殊福利金' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SQQT' '税前其他' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'PSJBXSS' '平时加班小时数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'PSJBF' '平时加班费' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZMJBXSS' '周末加班小时数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZMJBF' '周末加班费' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'FDJJRJBXS' '法定节假日加班小时数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'FDJJRJBF' '法定节假日加班费' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SJSS' '事假时数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SJKK' '事假扣款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BJSS' '病假时数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BJKK' '病假扣款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'KGSS' '旷工时数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'KGKK' '旷工扣款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CJSS' '产假时数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
  PERFORM FILL_FIELDCAT USING 'CJKK' '产假扣款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CZDAYC' '迟到/早退/打卡异常扣款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YFGZ' '应发工资总额' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GJJ_GR ' '公积金-个人' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YLAOBX_GR' '养老保险-个人' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SYBX_GR' '失业保险-个人' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YLBX_GR' '医疗保险-个人' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DBYLBX_GR' '大病医疗保险-个人' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 07.06.2017 13:23:48  BEGIN
  PERFORM FILL_FIELDCAT USING 'ZDJBYLBZ_GR' '重大疾病医疗补助-个人' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SBTZ_GR' '社保调整-个人' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GJJTZ_GR' '公积金调整-个人' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 07.06.2017 13:23:48  END

*&--代码添加 BY HANDYBY 15.06.2017 14:48:43  BEGIN
  PERFORM FILL_FIELDCAT USING 'SBHJ_GR' '社保合计（个人）' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 15.06.2017 14:48:43  END

  PERFORM FILL_FIELDCAT USING 'FDFL_GR' '法定福利个人扣发合计' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GJJ_GS' '公积金-公司' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YLAOBX_GS' '养老保险-公司' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SYBX_GS' '失业保险-公司' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YLBX_GS' '医疗保险-公司' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DBYLBX_GS' '大病医疗保险-公司' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GSBX_GS' '工伤保险-公司' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SYUBX_GS' '生育保险-公司' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 07.06.2017 13:23:48  BEGIN
  PERFORM FILL_FIELDCAT USING 'ZDJBYLBZ_GS' '重大疾病医疗补助-公司' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YWBX_GS' '意外保险-公司' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 07.06.2017 13:23:48  END

*&--代码添加 BY HANDYBY 15.06.2017 14:48:43  BEGIN
  PERFORM FILL_FIELDCAT USING 'SBHJ_GS' '社保合计（公司）' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 15.06.2017 14:48:43  END

  PERFORM FILL_FIELDCAT USING 'FDFL_GS' '法定福利公司部分合计' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'XJJS' '现金计税' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YSGZZE' '应税工资总额' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'EETYP_TXT' '计税类型' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'EXPAM' '免税额' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'TXPCT' '税率' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'TXACC' '速算扣除数' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GRSDS' '个人所得税' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JSHGZZE' '计税后工资总额' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SSZSF' '宿舍住宿费' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SSSDF' '宿舍水电费' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GSFK' '公司罚款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JK' '借款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GYJK' '公益捐款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CRMKK' 'CRM扣款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'CFKK' '餐费扣款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SJQFKK' '手机欠费扣款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'LZGYF' '离职工衣费' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DDBZJ' '电脑保证金' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SHQT' '税后其他' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'SHHJHZE' '税后合计后总额' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BFGZ' '补发工资' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'HFGZ' '缓发工资' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'NZJJ' '年终奖金' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'NZJS' '年终奖金税' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'LZBCJ' '离职补偿金' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'LZBCJS' '离职补偿金税' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'DYSFGZ' '当月实发工资' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZDGZBZ' '最低工资标准' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'GZBZ' '工资备注' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 07.06.2017 16:58:19  BEGIN
  PERFORM FILL_FIELDCAT USING 'DDTEXT' '工资成本归属单位' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 07.06.2017 16:58:19  END

  IF P_PAYTY = 'A'.
    PERFORM FILL_FIELDCAT USING 'NZJJBZ' '年终奖金备注' SPACE SPACE  8 SPACE SPACE SPACE.
    APPEND W_FCAT TO GT_FCAT.
  ENDIF.

  PERFORM FILL_FIELDCAT USING 'BYQK' '本月欠款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

*&--代码添加 BY HANDYBY 29.06.2017 18:25:56  BEGIN
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

  PERFORM FILL_FIELDCAT USING 'GLJTBZ_XS' '管理津贴标准-显示' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'ZWJTBZ_XS' '职位津贴标准-显示' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'JSJTBZ_XS' '技术津贴标准-显示' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'FJJBXZBZ_XS' '附加基本薪资标准-显示' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BTJTBZ_XS' '补贴津贴标准-显示' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
*&--代码添加 BY HANDYBY 29.06.2017 18:25:56  END

  PERFORM FILL_FIELDCAT USING 'QYQK' '前月欠款' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'YNSSDE' '应纳税所得额（应税工资总额-免税额）' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'QQKC' '缺勤扣除' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.

  PERFORM FILL_FIELDCAT USING 'BTHJBZ_JD' '补贴合计标准（金达）' SPACE SPACE  8 SPACE SPACE SPACE.
  APPEND W_FCAT TO GT_FCAT.
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
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'ALV_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     i_grid_title             = l_title
*     I_GRID_SETTINGS          =
      IS_LAYOUT_LVC            = W_LAYOUT
      IT_FIELDCAT_LVC          = GT_FCAT
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
      IS_VARIANT               = ALV_VARIANT
"     I_DEFAULT                = 'X'
      I_SAVE                   = 'A'
*     IS_VARIANT               =
*     it_events                = gt_events
*     it_event_exit            = gt_event_exit
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
*   IMPORTING
*     E_exit_CAUSED_BY_CALLER  =
*     ES_exit_CAUSED_BY_USER   =
    TABLES
      T_OUTTAB                 = GT_OUT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
*       GUI状态设置
*----------------------------------------------------------------------*
*      -->RT_EXTAB   GUI状态设置
*----------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_SCREEN' ."EXCLUDING RT_EXTAB.
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
  IF FU_NAME = 'PERNR'.
    W_FCAT-LZERO = 'X'.
  ENDIF.
  IF FU_NAME = 'YFGZ'.
    W_FCAT-EMPHASIZE = 'C600'.
  ENDIF.

  IF FU_NAME = 'YSGZZE'.
    W_FCAT-EMPHASIZE = 'C300'.
  ENDIF.

  IF FU_NAME = 'DYSFGZ'.
    W_FCAT-EMPHASIZE = 'C500'.
  ENDIF.

  IF FU_NAME = 'HFGZ'.
    W_FCAT-EMPHASIZE = 'C601'.
  ENDIF.
ENDFORM.                    "fill_fieldcat
*&---------------------------------------------------------------------*
*&      Form  PROCESS_PAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LFS_PA0001>_PERNR  text
*----------------------------------------------------------------------*
FORM PROCESS_PAY_DATA  USING FU_PERNR TYPE PA0001-PERNR.
  DATA: LT_RGDIR  TYPE STANDARD TABLE OF PC261,
        LT_PAYCN  TYPE STANDARD TABLE OF PAYCN_RESULT,      "工资核算结果 (中国)
        L_RELID   TYPE                   RELID_PCL,         "在表 PCLx 中簇的区域标识
        L_MOLGA   TYPE                   MOLGA,             "国家分组
        L_INDEX   TYPE                   SY-TABIX,          "索引，用于找最近调整前的工序序列处理差异
        L_SEQNR   TYPE                   PC261-SEQNR,
        L_SEQNR_C TYPE                   PC261-SEQNR,
        L_BEGDA   TYPE                   SY-DATUM,          "薪资月份的第一天
        L_ENDDA   TYPE                   SY-DATUM,          "薪资月份的最后一天
        L_ADD     TYPE                   C,                 "增加
        L_BEGDA_N TYPE                   SY-DATUM,          "下个月初
        L_MONTH   TYPE                   T5A4A-DLYMO,       "月份
        L_YEAR    TYPE                   T5A4A-DLYYR,       "年份
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
  LOOP AT LT_RGDIR ASSIGNING <LFS_PC261> WHERE PAYTY = P_PAYTY
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
           L_BEGDA.

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

    "取核算类型描述
    PERFORM GET_DOMVALUE_TEXT USING 'PAYTY'
             <LFS_PC261>-PAYTY
              LW_OUT-GZHSL.
    IF <LFS_PC261>-PAYTY = 'A'.
      LW_OUT-IPEND = <LFS_PC261>-IPEND.     "年终奖金核算日期

      LOOP AT GT_PA0267  ASSIGNING <FS_PA0267> WHERE PERNR = FU_PERNR AND ( BEGDA <= LW_OUT-IPEND AND ENDDA >= LW_OUT-IPEND ).
        CONCATENATE <FS_PA0267>-ZZHRBZ01 <FS_PA0267>-ZZHRBZ02 INTO LW_OUT-NZJJBZ .   "年终奖金备注
        EXIT.
      ENDLOOP.
    ENDIF.

    "取核算标识
    LW_OUT-PAYID  =  <LFS_PC261>-PAYID.
    "更新员工的其它信息
    PERFORM UPDATE_PERSON_INFOR USING LW_OUT
                                      L_BEGDA
                                      L_ENDDA
                                      L_ADD
                                      L_RELID
                                      L_MOLGA.
    IF S_TRFGB[] IS NOT INITIAL.
      IF LW_OUT-TRFGB NE S_TRFGB-LOW .   "判读获取更新员工 薪资区域 若 不等于 选择屏幕的 薪资等级 就排除 ，不进行后续处理
        CONTINUE.
      ENDIF.
    ENDIF.
    IF L_ADD = SPACE.
      CONTINUE.
    ENDIF.
*     "下月初 = 本月末 + 1
*    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*      EXPORTING
*        DATE      = L_ENDDA
*        DAYS      = 1
*        MONTHS    = L_MONTH
*        SIGNUM    = '+'
*        YEARS     = L_YEAR
*      IMPORTING
*        CALC_DATE = L_BEGDA_N.
*
*    "工资发放年月
*    LW_OUT-FFMON = L_BEGDA_N(6).

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
                                               AND FPPER < <LFS_PC261>-FPPER           "fpper要调整工资的月份
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
        "check 当前月调整前纪录的数据月 和 当前月调整纪录的数据月是否相等
        IF <LFS_PC261_B>-FPPER EQ <LFS_PC261_C>-FPPER .
          L_SEQNR_C  = <LFS_PC261_B>-SEQNR.
        ELSE.
          CLEAR:L_SEQNR_C .
        ENDIF.
        "     L_SEQNR_C  = <LFS_PC261_B>-SEQNR.
      ELSE.
        "    CONTINUE.
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
FORM UPDATE_PERSON_INFOR USING FU_OUT   TYPE TY_OUT
                                FU_BEGDA TYPE SY-DATUM
                                FU_ENDDA TYPE SY-DATUM
                                FU_ADD   TYPE C
                                FU_RELID TYPE RELID_PCL "在表 PCLx 中簇的区域标识
                                FU_MOLGA TYPE MOLGA.   "国家分组

  DATA: L_DAR_FIELD TYPE STRING,
        L_DAT_FIELD TYPE STRING,
        L_NUM       TYPE N LENGTH 2,
        L_STAT2     TYPE PA0000-STAT2,
        L_PLANS     TYPE PA0001-PLANS,
        L_ENDDA     TYPE SY-DATUM.

  FIELD-SYMBOLS: <LFS_DAR_FIELD>,
                 <LFS_DAT_FIELD>,
  <LFS_PA0001>      TYPE PA0001.
  SORT GT_T501T    BY PERSG.
  SORT GT_T503T    BY PERSK.
  SORT GT_PA0185   BY PERNR SUBTY.
  SORT GT_T529U    BY STATV.
  SORT GT_HRP1000  BY OBJID.
  SORT GT_PA0041   BY PERNR.
  SORT GT_PA0000   BY PERNR.
  SORT GT_T500P    BY PERSA.
  SORT GT_T510G    BY MOLGA TRFGB.

  CLEAR FU_ADD.

*&--代码添加 BY HANDYBY 09.06.2017 18:47:50  BEGIN
  DATA:BEGIN OF LS_DD07T,
         DOMNAME    TYPE DD07T-DOMNAME,
         DDLANGUAGE TYPE DD07T-DDLANGUAGE,
         AS4LOCAL   TYPE DD07T-AS4LOCAL,
         VALPOS     TYPE DD07T-VALPOS,
         AS4VERS    TYPE DD07T-AS4VERS,
         DDTEXT     TYPE DD07T-DDTEXT,
       END OF LS_DD07T .
  DATA LT_DD07T LIKE TABLE OF LS_DD07T .

  SELECT DOMNAME
         DDLANGUAGE
         AS4LOCAL
         VALPOS
         AS4VERS
         DDTEXT
    INTO CORRESPONDING FIELDS OF TABLE LT_DD07T
    FROM DD07T
   WHERE DOMNAME = 'ZDOHRCBGS'
     AND DDLANGUAGE = SY-LANGU
     AND AS4LOCAL = 'A'.
  SORT LT_DD07T BY VALPOS .
*&--代码添加 BY HANDYBY 09.06.2017 18:47:50  END

  LOOP AT GT_PA0001 ASSIGNING <FS_PA0001> WHERE PERNR = FU_OUT-PERNR
                                            AND BEGDA <= FU_ENDDA
                                            AND ENDDA >= FU_ENDDA.

    "权限检查
*&--代码注释 BY HANDYBY 19.06.2017 17:01:11  BEGIN
*    PERFORM CHECK_AUTH USING <FS_PA0001>-VDSK1
*                             FU_ADD.
*&--代码注释 BY HANDYBY 19.06.2017 17:01:11  END
*&--代码添加 BY HANDYBY 19.06.2017 17:01:18  BEGIN
    PERFORM CHECK_AUTH USING <FS_PA0001>-ABKRS
                               FU_ADD.
*&--代码添加 BY HANDYBY 19.06.2017 17:01:18  END

    IF FU_ADD = SPACE.
      CONTINUE.
    ENDIF.
    FU_ADD  = 'X'.

    CLEAR: L_STAT2,
           L_PLANS,
           L_ENDDA.
    "前一条记录的结束日期
    L_ENDDA = <FS_PA0001>-BEGDA - 1.

    "员工姓名
    FU_OUT-SNAME           = <FS_PA0001>-SNAME.
    FU_OUT-KOSTL           = <FS_PA0001>-KOSTL.
    " CONDENSE FU_OUT-SNAME NO-GAPS.

    "员工组
    READ TABLE  GT_T501T ASSIGNING <FS_T501T> WITH KEY PERSG = <FS_PA0001>-PERSG BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      FU_OUT-PERSG_PTEXT  = <FS_T501T>-PTEXT.
    ENDIF.

    "员工子组
    READ TABLE GT_T503T ASSIGNING <FS_T503T> WITH KEY PERSK = <FS_PA0001>-PERSK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      FU_OUT-PERSK_PTEXT   = <FS_T503T>-PTEXT.
    ENDIF.


    "工资核算范围
    READ TABLE GT_T549T ASSIGNING <FS_T549T> WITH KEY ABKRS = <FS_PA0001>-ABKRS BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      FU_OUT-ATEXT   = <FS_T549T>-ATEXT.
    ENDIF.

    "公司代码名称
    READ TABLE GT_T001 ASSIGNING <FS_T001> WITH KEY BUKRS = <FS_PA0001>-BUKRS.
    IF SY-SUBRC = 0.
      FU_OUT-BUKRS_BUTXT = <FS_T001>-BUTXT .
    ENDIF.

    "薪资等级区域
    LOOP AT GT_PA0008 ASSIGNING <FS_PA0008> WHERE PERNR = <FS_PA0001>-PERNR
                                              AND BEGDA <= FU_ENDDA
                                              AND ENDDA >= FU_ENDDA.
      FU_OUT-TRFGB = <FS_PA0008>-TRFGB .
      " 薪资等级区域文本
      READ TABLE GT_T510G  ASSIGNING <FS_T510G>  WITH KEY MOLGA = FU_MOLGA TRFGB =  FU_OUT-TRFGB.
      IF SY-SUBRC = 0.
        FU_OUT-TRFGB_TGBTX = <FS_T510G>-TGBTX .
      ENDIF.
    ENDLOOP.

    "雇佣状态
    LOOP AT GT_PA0000 ASSIGNING <FS_PA0000> WHERE PERNR = <FS_PA0001>-PERNR
                                              AND BEGDA <= FU_ENDDA
                                              AND ENDDA >= FU_ENDDA.

      L_STAT2 = <FS_PA0000>-STAT2.

*      READ TABLE GT_T529U ASSIGNING <FS_T529U> WITH KEY STATV = <FS_PA0000>-STAT1 BINARY SEARCH.
*      IF SY-SUBRC EQ 0.
*        FU_OUT-GYZT        = <FS_T529U>-TEXT1.
*      ENDIF.

      EXIT.
    ENDLOOP.

    "工资核算范围
    FU_OUT-ABKRS            = <FS_PA0001>-ABKRS.

*    "部门
*    LOOP AT GT_HRP1000 ASSIGNING <FS_HRP1000> WHERE OBJID = <FS_PA0001>-ORGEH
*                                            AND BEGDA <= FU_ENDDA
*                                            AND ENDDA >= FU_ENDDA.
*
*      FU_OUT-ORGEH            = <FS_HRP1000>-STEXT.
*
*      EXIT.
*    ENDLOOP.

*       "取一级部门信息
*    PERFORM GET_HIERARCHY_UP USING FU_OUT-DW
*                                  <FS_PA0001>-ORGEH
*                                  FU_ENDDA.
    "取集团、控股一二三级组织部门信息
    DATA:STRU_TAB LIKE TABLE OF   QCAT_STRU WITH HEADER LINE.
    DATA:OBJID TYPE  HRP1000-OBJID.
    OBJID = <FS_PA0001>-ORGEH .
    CALL FUNCTION 'RHPH_STRUCTURE_READ'
      EXPORTING
        PLVAR             = '01'
        OTYPE             = 'O'
        OBJID             = OBJID
        WEGID             = 'A002'
        BEGDA             = FU_BEGDA
        ENDDA             = FU_ENDDA
        PUP_INFO          = 'X'
        WITH_STEXT        = 'X'
 "      TDEPTH            =
      TABLES
        STRU_TAB          = STRU_TAB
      EXCEPTIONS
        CATALOGUE_PROBLEM = 1
        ROOT_NOT_FOUND    = 2
        WEGID_NOT_FOUND   = 3
        OTHERS            = 4.
    IF SY-SUBRC <> 0.

* IMPLEMENT SUITABLE ERROR HANDLING HERE
    ELSE.
      LOOP AT STRU_TAB.
        IF STRU_TAB-SHORT = 'C4'.

          FU_OUT-C40RGEN  =  STRU_TAB-STEXT.
        ENDIF.
        IF STRU_TAB-SHORT = 'C3'.
          FU_OUT-C30RGEN  =  STRU_TAB-STEXT.
        ENDIF.
        IF STRU_TAB-SHORT = 'C2'.
          FU_OUT-C20RGEN  =  STRU_TAB-STEXT.
        ENDIF.
        IF  STRU_TAB-SHORT = 'C1'.
          FU_OUT-C10RGEN  =  STRU_TAB-STEXT.
        ENDIF.
        IF  STRU_TAB-SHORT = 'B0'.
          FU_OUT-KGTEXT  =  STRU_TAB-STEXT.
        ENDIF.
        IF  STRU_TAB-SHORT = 'A0'.
          FU_OUT-JTTEXT  =  STRU_TAB-STEXT.
        ENDIF.
        IF STRU_TAB-SHORT = ''.
          READ TABLE GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OTYPE = 'O' OBJID = STRU_TAB-OBJID ."SHORT = 'C4'.
          IF SY-SUBRC = 0.
            CASE <FS_HRP1000>-SHORT .
              WHEN 'A0'.
                FU_OUT-JTTEXT   = STRU_TAB-STEXT .
              WHEN 'B0'.
                FU_OUT-KGTEXT  = STRU_TAB-STEXT.
              WHEN 'C1' .
                FU_OUT-C10RGEN  =  STRU_TAB-STEXT.
              WHEN 'C2'.
                FU_OUT-C20RGEN  =  STRU_TAB-STEXT.
              WHEN 'C3'.
                FU_OUT-C30RGEN  =  STRU_TAB-STEXT.
              WHEN 'C4'.
                FU_OUT-C40RGEN  =  STRU_TAB-STEXT.
            ENDCASE.
          ENDIF.

        ENDIF.
      ENDLOOP.

    ENDIF.


    "岗位
    IF L_STAT2 = '0'.   "若员工已离职，则输出上一条PA0001_PLANS最新的职位
      READ TABLE GT_PA0001 ASSIGNING <LFS_PA0001> WITH KEY PERNR = <FS_PA0001>-PERNR
                                                           ENDDA = L_ENDDA.
      IF SY-SUBRC EQ 0.
        L_PLANS = <LFS_PA0001>-PLANS.
      ENDIF.
      LOOP AT GT_HRP1000 ASSIGNING <FS_HRP1000> WHERE OBJID = L_PLANS
                                          AND BEGDA <= L_ENDDA
                                          AND ENDDA >= L_ENDDA
                                          AND OTYPE = 'S'.
        FU_OUT-PLANS_TXT            = <FS_HRP1000>-STEXT.
        EXIT.
      ENDLOOP.
    ELSE.
      L_PLANS = <FS_PA0001>-PLANS.
      LOOP AT GT_HRP1000 ASSIGNING <FS_HRP1000> WHERE OBJID = L_PLANS
                                          AND BEGDA <= FU_ENDDA
                                          AND ENDDA >= FU_ENDDA
                                           AND OTYPE = 'S'..
        FU_OUT-PLANS_TXT            = <FS_HRP1000>-STEXT.
        EXIT.
      ENDLOOP.
    ENDIF.


    "LOOP AT GT_HRP1001 ASSI




    "兼职岗位
    IF L_STAT2 NE'0'.   "若员工未离职
      LOOP AT  GT_HRP1001 ASSIGNING <FS_HRP1001> WHERE OBJID = <FS_PA0001>-PERNR
                                                 AND BEGDA <= FU_ENDDA
                                           AND ENDDA >= FU_ENDDA.
        IF <FS_HRP1001>-OBJID NE ''.

          LOOP  AT GT_HRP1000 ASSIGNING <FS_HRP1000> WHERE OBJID = <FS_HRP1001>-SOBID
                                              AND BEGDA <= FU_ENDDA
                                              AND ENDDA >= FU_ENDDA.

            CONCATENATE  FU_OUT-PLANS_TXT '兼' <FS_HRP1000>-STEXT INTO  FU_OUT-PLANS_TXT .
            EXIT.
          ENDLOOP.
        ENDIF.

      ENDLOOP.
    ELSE.
      LOOP AT  GT_HRP1001 ASSIGNING <FS_HRP1001> WHERE OBJID = <FS_PA0001>-PERNR
                                                AND  ENDDA = L_ENDDA.

        IF <FS_HRP1001>-OBJID NE ''.

          LOOP  AT GT_HRP1000 ASSIGNING <FS_HRP1000> WHERE OBJID = <FS_HRP1001>-SOBID
                                              AND BEGDA <= L_ENDDA
                                              AND ENDDA >= L_ENDDA.

            CONCATENATE  FU_OUT-PLANS_TXT '兼' <FS_HRP1000>-STEXT INTO  FU_OUT-PLANS_TXT .
            EXIT.
          ENDLOOP.
        ENDIF.

      ENDLOOP.
    ENDIF.


    "身份证号
    LOOP AT GT_PA0185 ASSIGNING <FS_PA0185> WHERE PERNR = <FS_PA0001>-PERNR
                                              AND  ( ICTYP = '01' OR  ICTYP = 'Z1')
                                              AND BEGDA <= FU_ENDDA
                                              AND ENDDA >= FU_ENDDA.
      IF <FS_PA0185>-ICTYP = '01'  .
        FU_OUT-ICNUM          = <FS_PA0185>-ICNUM.       "身份证
      ENDIF.
      IF <FS_PA0185>-ICTYP = 'Z1'  .
        FU_OUT-ICNUMZ1        = <FS_PA0185>-ICNUM .   "外籍护照号
      ENDIF.
      EXIT.
    ENDLOOP.

    "工资卡银行名称、工资卡号

    LOOP AT GT_PA0009 ASSIGNING <FS_PA0009>  WHERE PERNR = <FS_PA0001>-PERNR
                                                  AND BEGDA <= FU_ENDDA
                                                AND ENDDA >= FU_ENDDA.
      FU_OUT-ZZGZYH = <FS_PA0009>-ZZGZYH. "工资卡银行长城
      FU_OUT-ZZGZZH = <FS_PA0009>-ZZGZZH.  "工资卡号
*&--代码添加 BY HANDYBY 22.06.2017 10:15:26  BEGIN
      FU_OUT-ZHMC = <FS_PA0009>-ZZZHMC.  "支行名称
*&--代码添加 BY HANDYBY 22.06.2017 10:15:26  END
    ENDLOOP.

    "人事范围
    READ TABLE GT_T500P ASSIGNING <FS_T500P> WITH KEY PERSA = <FS_PA0001>-WERKS
                                                      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      FU_OUT-WERKS_NAME1 = <FS_T500P>-NAME1.
    ENDIF.

    "成本中心描述

    READ TABLE GT_CSKT ASSIGNING <FS_CSKT> WITH KEY KOSTL = <FS_PA0001>-KOSTL BINARY SEARCH.

    IF SY-SUBRC EQ 0 .
      FU_OUT-KOSTL_KTEXT = <FS_CSKT>-KTEXT.
    ENDIF.
    "更新备注信息
    LOOP AT GT_PA9128 ASSIGNING <FS_PA9128> WHERE PERNR = FU_OUT-PERNR
                                          "     AND ZZBZLX = 'Z1'
                                              AND BEGDA <= FU_ENDDA
                                              AND ENDDA >= FU_ENDDA.
      CONCATENATE <FS_PA9128>-ZZBZ0  <FS_PA9128>-ZZBZ1 <FS_PA9128>-ZZBZ2 <FS_PA9128>-ZZBZ3 INTO  FU_OUT-GZBZ .
*      CONCATENATE  <FS_PA9128>-ZZBZX1 <FS_PA9128>-ZZBZX2 <FS_PA9128>-ZZBZX3 <FS_PA9128>-ZZBZX4 "<FS_PA9128>-ZZBZNR
*      INTO FU_OUT-BZ.

      EXIT.
    ENDLOOP.

*&--代码添加 BY HANDYBY 07.06.2017 16:50:48  BEGIN
    "取工资成本归属单位
    LOOP AT GT_PA9029 ASSIGNING <FS_PA9029> WHERE PERNR = FU_OUT-PERNR
                                              AND ENDDA = '99991231'.
      READ TABLE LT_DD07T INTO LS_DD07T WITH KEY VALPOS = <FS_PA9029>-ZZBZ0 BINARY SEARCH .
      IF SY-SUBRC = 0 .
        FU_OUT-DDTEXT = LS_DD07T-DDTEXT .
        CLEAR LS_DD07T .
        EXIT .
      ENDIF.
    ENDLOOP.
*&--代码添加 BY HANDYBY 07.06.2017 16:50:48  END

    "当前司龄起算日期（入职日期）
    LOOP AT GT_PA0041 ASSIGNING <FS_PA0041> WHERE  PERNR = FU_OUT-PERNR
                                           "     AND ZZBZLX = 'Z1'
                                               AND BEGDA <= FU_ENDDA
                                               AND ENDDA >= FU_ENDDA.
      CASE <FS_PA0041>-DAR01 .
        WHEN 'D1' .
          FU_OUT-RZRQ  = <FS_PA0041>-DAT01." 当前司龄起算日期（入职日期）
          EXIT.
      ENDCASE.
      CASE <FS_PA0041>-DAR02 .
        WHEN 'D1' .
          FU_OUT-RZRQ  = <FS_PA0041>-DAT02." 当前司龄起算日期（入职日期）
          EXIT.
      ENDCASE.
      CASE <FS_PA0041>-DAR03 .
        WHEN 'D1' .
          FU_OUT-RZRQ = <FS_PA0041>-DAT03." 当前司龄起算日期（入职日期）
          EXIT.
      ENDCASE.
      CASE <FS_PA0041>-DAR04 .
        WHEN 'D1' .
          FU_OUT-RZRQ = <FS_PA0041>-DAT04." 当前司龄起算日期（入职日期）
          EXIT.
      ENDCASE.
    ENDLOOP.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
FORM CHECK_AUTH  USING
*&--代码注释 BY HANDYBY 19.06.2017 17:02:19  BEGIN
*    FU_VDSK1 TYPE PA0001-VDSK1
*&--代码注释 BY HANDYBY 19.06.2017 17:02:19  END
*&--代码添加 BY HANDYBY 19.06.2017 17:02:25  BEGIN
  FU_ABKRS TYPE PA0001-ABKRS
*&--代码添加 BY HANDYBY 19.06.2017 17:02:25  END

                          FU_OK.

  FU_OK = 'X'.

*&--代码注释 BY HANDYBY 19.06.2017 17:01:38  BEGIN
*    AUTHORITY-CHECK OBJECT 'P_ORGIN' ID 'VDSK1' FIELD FU_VDSK1.
*&--代码注释 BY HANDYBY 19.06.2017 17:01:38  END
*&--代码添加 BY HANDYBY 19.06.2017 17:01:44  BEGIN
  AUTHORITY-CHECK OBJECT 'P_PCR' ID 'ABRKS' FIELD FU_ABKRS.
*&--代码添加 BY HANDYBY 19.06.2017 17:01:44  END
  IF SY-SUBRC NE 0.
    FU_OK = SPACE.
  ENDIF.

ENDFORM.                    " CHECK_AUTH


FORM GET_PAY_DATA USING FU_OUT   TYPE TY_OUT
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
                 <LFS_INT_FIELD> ,
                 <LFS_PAY>,
                 <LFS_PAY_C>.

*&--代码添加 BY HANDYBY 23.06.2017 14:11:30  BEGIN
*  DATA L_LFS_INT_FIELD TYPE P LENGTH 16 DECIMALS 2 .
*  ASSIGN L_LFS_INT_FIELD TO <LFS_INT_FIELD> .
*&--代码添加 BY HANDYBY 23.06.2017 14:11:30  END

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
  "  IF FU_SEQNR_C IS NOT INITIAL.
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

  " ENDIF.


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
            "排除 工资项 1012、1022、1032、1042、2032、2012、2022 、/I04回算月不汇总到本月合并
            IF  L_INTFIELD NE 'JCGZBZ' AND L_INTFIELD NE 'GWGZBZ' AND L_INTFIELD NE 'JXGZBZ'
               AND L_INTFIELD NE 'BTGZBZ' AND L_INTFIELD NE 'GWJTBZ' AND  L_INTFIELD NE 'JXJTBZ'
               AND L_INTFIELD NE  'JNJTBZ' AND  L_INTFIELD NE 'ZDGZBZ'.

              ASSIGN COMPONENT L_INTFIELD OF STRUCTURE <LFS_OUT> TO <LFS_INT_FIELD>.

              IF SY-SUBRC EQ 0.

                "当月工资 = 当月工资 + 其它月在本月调整的工资差异
                <LFS_INT_FIELD> = <LFS_INT_FIELD> + L_PAY_D.

              ELSE.

                "MESSAGE A007 WITH '<LFS_OUT>' L_INTFIELD.

              ENDIF.
            ENDIF.


          ENDIF.

          "对应历经期，将差异体现在独立一行
          IF P_LJQ = 'X'.

            ASSIGN COMPONENT L_INTFIELD OF STRUCTURE LW_OUT_D TO <LFS_INT_FIELD>.
            IF SY-SUBRC EQ 0.

              "工资项差异
*              CLEAR <LFS_INT_FIELD> .
*              ASSIGN L_LFS_INT_FIELD TO <LFS_INT_FIELD> .
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
*          CLEAR <LFS_INT_FIELD> .
*          ASSIGN L_LFS_INT_FIELD TO <LFS_INT_FIELD> .
          <LFS_INT_FIELD>  = <LFS_PAY>.
        ELSE.
          "MESSAGE A007 WITH 'FU_OUT' L_INTFIELD.
        ENDIF.

*******  读取税信息，只读取一条，理论上只有一条数据  *************
        READ TABLE LW_PAYCN-NAT-TAX ASSIGNING <LFS_TAX> INDEX 1.
        IF SY-SUBRC EQ 0.
          "计税类型文本
          ASSIGN COMPONENT 'EETYP_TXT' OF STRUCTURE FU_OUT TO <LFS_INT_FIELD>.
          IF <LFS_TAX>-EETYP = '0'. "中方员工工资税
*            CLEAR <LFS_INT_FIELD> .
*            ASSIGN L_LFS_INT_FIELD TO <LFS_INT_FIELD> .
            <LFS_INT_FIELD> = '中方员工工资税'.
          ENDIF.
          IF <LFS_TAX>-EETYP = '2'. "外籍员工工资税
*            CLEAR <LFS_INT_FIELD> .
*            ASSIGN L_LFS_INT_FIELD TO <LFS_INT_FIELD> .
            <LFS_INT_FIELD> = '外籍员工工资税'.
          ENDIF.
          IF <LFS_TAX>-EETYP = '4'. "劳务报酬计税
*            CLEAR <LFS_INT_FIELD> .
*            ASSIGN L_LFS_INT_FIELD TO <LFS_INT_FIELD> .
            <LFS_INT_FIELD> = '劳务报酬计税'.
          ENDIF.
          "工资税率
          ASSIGN COMPONENT 'TXPCT' OF STRUCTURE FU_OUT TO <LFS_INT_FIELD>.
          IF SY-SUBRC EQ 0.
*            CLEAR <LFS_INT_FIELD> .
*            ASSIGN L_LFS_INT_FIELD TO <LFS_INT_FIELD> .
            <LFS_INT_FIELD> = <LFS_TAX>-TXPCT.
          ENDIF.

          "速算扣除数
          ASSIGN COMPONENT 'TXACC' OF STRUCTURE FU_OUT TO <LFS_INT_FIELD>.
          IF SY-SUBRC EQ 0.
*            CLEAR <LFS_INT_FIELD> .
*            ASSIGN L_LFS_INT_FIELD TO <LFS_INT_FIELD> .
            <LFS_INT_FIELD> = <LFS_TAX>-TXACC.
          ENDIF.

          "免税额度
          ASSIGN COMPONENT 'EXPAM' OF STRUCTURE FU_OUT TO <LFS_INT_FIELD>.
          IF SY-SUBRC EQ 0.
*            CLEAR <LFS_INT_FIELD> .
*            ASSIGN L_LFS_INT_FIELD TO <LFS_INT_FIELD> .
            <LFS_INT_FIELD> = <LFS_TAX>-EXPAM.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.
  "对应差异的情况

  " IF FU_SEQNR_C IS NOT INITIAL.
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
          "  排除 工资项 1012、1022、1032、1042、2032、2012、2022 回算月不汇总到本月合并
          IF  L_INTFIELD NE 'JCGZBZ' AND L_INTFIELD NE 'GWGZBZ' AND L_INTFIELD NE 'JXGZBZ'
                AND L_INTFIELD NE 'BTGZBZ' AND L_INTFIELD NE 'GWJTBZ' AND  L_INTFIELD NE 'JXJTBZ'
               AND L_INTFIELD NE  'JNJTBZ'.

            ASSIGN COMPONENT L_INTFIELD OF STRUCTURE <LFS_OUT> TO <LFS_INT_FIELD>.
            IF SY-SUBRC EQ 0.

              "当月工资 = 当月工资 + 其它月在本月调整的工资差异
              <LFS_INT_FIELD> = <LFS_INT_FIELD> + L_PAY_D.

            ELSE.

              "MESSAGE A007 WITH '<LFS_OUT>' L_INTFIELD.

            ENDIF.

          ENDIF.


        ENDIF.

        "对应历经期，将差异体现在独立一行
        IF P_LJQ = 'X'.

          ASSIGN COMPONENT L_INTFIELD OF STRUCTURE LW_OUT_D TO <LFS_INT_FIELD>.
          IF SY-SUBRC EQ 0.

            "工资项差异
*            CLEAR <LFS_INT_FIELD> .
*            ASSIGN L_LFS_INT_FIELD TO <LFS_INT_FIELD> .
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
      LW_OUT_D-SNAME            = FU_OUT-SNAME.       "姓名
      LW_OUT_D-PERSG            = FU_OUT-PERSG.       "员工组
      LW_OUT_D-PERSG_PTEXT      = FU_OUT-PERSG_PTEXT.       "员工组
      LW_OUT_D-PERSK            = FU_OUT-PERSK.       "员工子组
      LW_OUT_D-PERSK_PTEXT      = FU_OUT-PERSK_PTEXT.       "员工子组
      LW_OUT_D-C10RGEN = FU_OUT-C10RGEN . "一级部门
      LW_OUT_D-C20RGEN = FU_OUT-C20RGEN . "二级部门
      LW_OUT_D-C30RGEN = FU_OUT-C30RGEN .  "三级部门
      LW_OUT_D-C40RGEN = FU_OUT-C40RGEN . "四级部门
      LW_OUT_D-PLANS_TXT = FU_OUT-PLANS_TXT."岗位
      "  LW_OUT_D-GYZT             = FU_OUT-GYZT.        "雇佣状态
      "   LW_OUT_D-RZDAT            = FU_OUT-RZDAT.       "入职日期
      "  LW_OUT_D-GZHSL            = FU_OUT-GZHSL.       "工资核算类型
      LW_OUT_D-ABKRS            = FU_OUT-ABKRS.       "工资核算范围代码
      LW_OUT_D-ATEXT            = FU_OUT-ATEXT.       "工资核算范围文本
      "    LW_OUT_D-ORGEH            = FU_OUT-ORGEH.       "部门
      "   LW_OUT_D-PLANS            = FU_OUT-PLANS.       "岗位
      "  LW_OUT_D-SFZH             = FU_OUT-SFZH.        "身份证号
      "   LW_OUT_D-FFMON            = FU_OUT-FFMON.       "发放年月
      "     LW_OUT_D-GZHSL            = FU_OUT-GZHSL.       "工资核算类型
      LW_OUT_D-WERKS            = FU_OUT-WERKS.       "人事范围
      LW_OUT_D-WERKS_NAME1 = FU_OUT-WERKS_NAME1.      "人事范围文本
      LW_OUT_D-TRFGB      = FU_OUT-TRFGB ."薪资等级区域
      LW_OUT_D-TRFGB_TGBTX = FU_OUT-TRFGB_TGBTX.     "薪资等级区域文本
      LW_OUT_D-RZRQ  = FU_OUT-RZRQ ." 当前司龄起算日期（入职日期）
      LW_OUT_D-KOSTL       = FU_OUT-KOSTL .           "成本中心
      LW_OUT_D-KOSTL_KTEXT = FU_OUT-KOSTL_KTEXT.      "成本中心文本
      LW_OUT_D-ICNUM       = FU_OUT-ICNUM ."身份证号
      LW_OUT_D-ICNUMZ1     = FU_OUT-ICNUMZ1."外籍护照号
      LW_OUT_D-ZZGZYH      = FU_OUT-ZZGZYH . "工资卡银行名称
      LW_OUT_D-ZZGZZH      = FU_OUT-ZZGZZH."工资卡号
      LW_OUT_D-BUKRS_BUTXT = FU_OUT-BUKRS_BUTXT ."公司名称
      "     LW_OUT_D-DW               = FU_OUT-DW.            "单位
      "    LW_OUT_D-ORGEH            = FU_OUT-ORGEH.       "部门
      "  LW_OUT_D-GZTX             = FU_OUT-GZTX.        "工资体系
      LW_OUT_D-CYFLG            = 'X'.                "标识差异行

      "插入到输出内表
      INSERT LW_OUT_D INTO GT_OUT INDEX L_INDEX.

    ENDIF.
  ELSE.

    FU_OUT-GZMON                = FU_GZMON.           "工资月份
    FU_OUT-JSMON                = FU_JSMON.           "计算月份

    "添加到输出内表
    APPEND FU_OUT TO GT_OUT.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LFS_RT>_LGART  text
*      -->P_L_VALUEFIELD  text
*      -->P_L_INTFIELD  text
*----------------------------------------------------------------------*
FORM GET_FIELDNAME USING L_LGART
                         L_VALUEFIELD
                         L_INTFIELD.
  CASE L_LGART .

    WHEN '9010' .
      L_VALUEFIELD = 'ANZHL'.
      " 应应计薪时数
      L_INTFIELD   = 'YJXSS'.

    WHEN '9030' .
      L_VALUEFIELD = 'ANZHL'.
      " 实计薪时数
      L_INTFIELD   = 'SJXSS'.

    WHEN '1012'.
      L_INTFIELD   = 'JCGZBZ'.  "基础工资标准
    WHEN '1010'.
      L_INTFIELD   = 'DYJCGZ'.  "当月基础工资
    WHEN '1022'.
      L_INTFIELD   = 'GWGZBZ'.  "岗位工资标准
    WHEN '1020'.
      L_INTFIELD   = 'DYGWGZ'.  "当月岗位工资
    WHEN '1032'.
      L_INTFIELD   = 'JXGZBZ'.  "绩效工资标准
    WHEN '1531'.
      L_VALUEFIELD = 'ANZHL'. "绩效工资系数-数量
      L_INTFIELD   = 'JXGZXS_1'.
    WHEN '1031'.
      L_INTFIELD   = 'JXGZXS'.  "绩效工资系数——金额
    WHEN '8010'.
      L_INTFIELD   = 'YWF'.  "业务费

    WHEN '1042'.
      L_INTFIELD   = 'BTGZBZ'.  "补贴工资标准

    WHEN '1040'.
      L_INTFIELD   = 'DYBTGZ'.  "当月补贴工资
    WHEN '2032'.
      L_INTFIELD   = 'GWJTBZ'.  "岗位津贴标准
    WHEN '2030'.
      L_INTFIELD   = 'GWJT'.  "岗位津贴
    WHEN '2012'.
      L_INTFIELD   = 'JXJTBZ'.  "绩效津贴标准
    WHEN '2010'.
      L_INTFIELD   = 'JXJT'.  "绩效津贴
    WHEN '2022'.
      L_INTFIELD   = 'JNJTBZ'.  "技能津贴标准
    WHEN '2020'.
      L_INTFIELD   = 'JNJT'.  "技能津贴
    WHEN '4010'.
      L_INTFIELD   = 'JJDJ'.  "计件单价
    WHEN '4020'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD   = 'JJGRGS'.  "计件个人工时
    WHEN '4030'.
      L_INTFIELD   = 'JJGZ'.  "计件工资
    WHEN '3090'.
      L_INTFIELD   = 'JJBFJ'.  "计件补发金
    WHEN '3320'.
      L_INTFIELD   = 'TSGWBT'.  "特殊岗位补贴

    WHEN '3100'.
      L_INTFIELD   = 'CFBZ'.  "餐费补助
    WHEN '3020'.
      L_INTFIELD   = 'GWBZ'.  "高温补助
    WHEN '3080'.
      L_INTFIELD   = 'CCBZ'.  "出差补助
    WHEN '6010'.
      L_INTFIELD   = 'HFBZ'.  "话费补助
    WHEN '3350'.
      L_INTFIELD   = 'GLJ'.  "工龄奖
    WHEN '3070'.
      L_INTFIELD   = 'QQJ'.  "全勤奖
    WHEN '2110'.
      L_INTFIELD   = 'XMBZYBZ'.  "项目补助月标准
    WHEN '2111'.
      L_INTFIELD   = 'XMBZRBZ'.  "项目补助日标准
    WHEN '2112'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD   = 'XMGZTS'.  "项目工作天数
    WHEN '2113'.
      L_INTFIELD   = 'DYXMBZ'.  "当月项目补助
    WHEN '3310'.
      L_INTFIELD   = 'JXKHZJ'.  "绩效考核增减
    WHEN '3120'.
      L_INTFIELD   = 'JBBZ'.  "加班补助
    WHEN '5000'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD   = 'YBS'.  "夜班数
    WHEN '3130'.

      L_INTFIELD   = 'YBJT'.  "夜班津贴
    WHEN '3160'.
      L_INTFIELD   = 'JXGZYKJ'.  "绩效工资预扣减
    WHEN '3170'.
      L_INTFIELD   = 'DNBT'.  "电脑补贴
    WHEN '3050'.
      L_INTFIELD   = 'SSZBT'.  "宿舍长补贴
    WHEN '3010'.
      L_INTFIELD   = 'GKBZ'.  "高空补助
    WHEN '3030'.
      L_INTFIELD   = 'XSTC'.  "销售提成
    WHEN '6020'.
      L_INTFIELD   = 'KZJJ'.  "考证奖金
    WHEN '6130'.
      L_INTFIELD   = 'QTJJ'.  "其他奖金
    WHEN '3250'.
      L_INTFIELD   = 'LBYPJE'.  "劳保用品金额
    WHEN '3280'.
      L_INTFIELD   = 'JRFL'.  "节日福利
    WHEN '3330'.
      L_INTFIELD   = 'TSFLJ'.  "特殊福利金
    WHEN '6060'.
      L_INTFIELD   = 'SQQT'.  "税前其他
    WHEN '5015'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD   = 'PSJBXSS'.  "平时加班小时数
    WHEN '5515'.

      L_INTFIELD   = 'PSJBF'.  "平时加班费

    WHEN '5020'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD   = 'ZMJBXSS'.  "周末加班小时数
    WHEN '5520'.
      L_INTFIELD   = 'ZMJBF'.  "周末加班费
    WHEN '5030'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD   = 'FDJJRJBXS'.  "法定节假日加班小时数
    WHEN '5530'.
      L_INTFIELD   = 'FDJJRJBF'.  "法定节假日加班费
    WHEN '5130'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD   = 'SJSS'.  "事假时数
    WHEN '5131'.
      L_INTFIELD   = 'SJKK'.  "事假扣款
    WHEN '5160'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD   = 'BJSS'.  "病假时数
    WHEN '5161'.
      L_INTFIELD   = 'BJKK'.  "病假扣款
    WHEN '5180'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD   = 'CJSS'.  "产假时数
    WHEN '5180'.

      L_INTFIELD   = 'CJKK'.  "产假扣款
    WHEN '5110'.
      L_INTFIELD   = 'CZDAYC'.  "迟到/早退/打卡异常扣款
    WHEN '8363'.
      L_INTFIELD   = 'GJJ_GR'.  "公积金-个人
    WHEN '8313'.
      L_INTFIELD   = 'YLAOBX_GR'.  "养老保险-个人
    WHEN '8323'.
      L_INTFIELD   = 'SYBX_GR'.  "失业保险-个人
    WHEN '8333'.
      L_INTFIELD   = 'YLBX_GR'.  "医疗保险-个人
    WHEN '8373'.
      L_INTFIELD   = 'DBYLBX_GR'.  "大病医疗保险-个人
    WHEN '8364'.
      L_INTFIELD   = 'GJJ_GS'.  "公积金-公司
    WHEN '8314'.
      L_INTFIELD   = 'YLAOBX_GS'.  "养老保险-公司
    WHEN '8324'.
      L_INTFIELD   = 'SYBX_GS'.  "失业保险-公司
    WHEN '8334'.
      L_INTFIELD   = 'YLBX_GS'.  "医疗保险公司
    WHEN '8374'.
      L_INTFIELD   = 'DBYLBX_GS'.  "大病医疗保险-公司
    WHEN '8344'.
      L_INTFIELD   = 'GSBX_GS'.  "工伤保险-公司
    WHEN '8354'.
      L_INTFIELD   = 'SYUBX_GS'.  "生育保险-公司
    WHEN '6150'.
      L_INTFIELD   = 'XJJS'.  "现金计税
    WHEN '/401'.
      L_INTFIELD   = 'YSGZZE'.  "应税工资总额
    WHEN '/403'.
      L_INTFIELD   = 'GRSDS'.  "个人所得税
    WHEN '7020'.
      L_INTFIELD   = 'SSZSF'.  "宿舍住宿费
    WHEN '7000'.
      L_INTFIELD   = 'SSSDF'.  "宿舍水电费
    WHEN '7030'.
      L_INTFIELD   = 'GSFK'.  "公司罚款
    WHEN '7050'.
      L_INTFIELD   = 'JK'.  "借款
    WHEN '7010'.
      L_INTFIELD   = 'GYJK'.  "公益捐款
    WHEN '7040'.
      L_INTFIELD   = 'CRMKK'.  "CRM扣款
    WHEN '7080'.
      L_INTFIELD   = 'CFKK'.  "餐费扣减
    WHEN '7090'.
      L_INTFIELD   = 'SJQFKK'.  "手机欠费扣减
    WHEN '7060'.
      L_INTFIELD   = 'LZGYF'.  "离职工衣费
    WHEN '7070'.
      L_INTFIELD   = 'DDBZJ'.  "电脑保证金
    WHEN '7100'.
      L_INTFIELD   = 'SHQT'.  "税后其他
    WHEN '7120'.
      L_INTFIELD   = 'BFGZ'.  "补发工资
    WHEN '7130'.
      L_INTFIELD   = 'HFGZ'.  "缓发工资
    WHEN '6160'.
      L_INTFIELD   = 'NZJJ'.  "年终奖金
    WHEN '/404'.
      L_INTFIELD   = 'NZJS'.  "年终奖税
    WHEN '6200'.
      L_INTFIELD   = 'LZBCJ'.  "离职补偿金
    WHEN '/408'.
      L_INTFIELD   = 'LZBCJS'.  "离职补偿金税
    WHEN '/560'.
      L_INTFIELD   = 'DYSFGZ'.  "当月实发工资
    WHEN '/I04'.
      L_INTFIELD   = 'ZDGZBZ'.  "最低工资标准
    WHEN '1030'.
      L_INTFIELD   = 'JXGZBZ_1'. "绩效工资标准
    WHEN '3061'.
      L_INTFIELD   = 'ZFBZ_1'. "住房补助（70小时内）
    WHEN '3065'.
      L_INTFIELD   = 'ZFBT_1'. "住房补助（70小时内）
    WHEN '5175'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD  = 'KGSS_1'."数量  旷工时数（光电+金达）
    WHEN '5176'.
      L_VALUEFIELD = 'ANZHL'.
      L_INTFIELD  = 'KGSS_2'."数量  旷工时数（深圳LYD+电视）
    WHEN '5575'.
      L_INTFIELD  = 'KGKK_1'."金额  旷工时数（光电+金达）
    WHEN '5576'.
      L_INTFIELD  = 'KGKK_2'."金额  旷工时数（深圳LYD+电视）
    WHEN '/101'.
      L_INTFIELD  = 'ZJE_1' ."总金额 /101
    WHEN '/561'.
      L_INTFIELD  = 'BYQK' ."本月欠款 /101
    WHEN '/563'.
      L_INTFIELD  = 'QYQK' ."前月欠款/101

*&--代码添加 BY HANDYBY 07.06.2017 12:52:52  BEGIN
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
*&--代码添加 BY HANDYBY 07.06.2017 12:52:52  END

*&--代码添加 BY HANDYBY 12.06.2017 09:52:17  BEGIN
    WHEN '9015'.
      L_INTFIELD  = 'XLBT_JD' ."学历补贴(金达)
    WHEN '9016'.
      L_INTFIELD  = 'ZYZGBT_JD' ."执业资格补贴(金达)
    WHEN '9017'.
      L_INTFIELD  = 'JSZCBT_JD' ."技术职称补贴(金达)
    WHEN '9018'.
      L_INTFIELD  = 'JTBT_JD' ."交通补贴(金达)
    WHEN '9019'.
      L_INTFIELD  = 'HFBT_JD' ."话费补贴(金达)
    WHEN '9020'.
      L_INTFIELD  = 'CFBZ_JD' ."餐费补助(金达)
    WHEN '9021'.
      L_INTFIELD  = 'ZFBT_JD' ."住房补贴(金达)
*    WHEN '9014'.
*      L_INTFIELD  = 'YWBX_GS' ."意外保险(公司)
    WHEN '9085'.
      L_INTFIELD  = 'XLBTBZXS_JD' ."学历补贴标准-显示(金达)
    WHEN '9086'.
      L_INTFIELD  = 'ZYZGBTBZXS_JD' ."执业资格补贴标准-显示(金达)
    WHEN '9087'.
      L_INTFIELD  = 'JSZCBTBZXS_JD' ."技术职称补贴标准-显示(金达)
    WHEN '9088'.
      L_INTFIELD  = 'JTBTBZXS_JD' ."交通补贴标准-显示(金达)
    WHEN '9089'.
      L_INTFIELD  = 'HFBTXS_JD' ."话费补贴-显示(金达)
    WHEN '9090'.
      L_INTFIELD  = 'CFBZXS_JD' ."餐费补助-显示(金达)
    WHEN '9091'.
      L_INTFIELD  = 'ZFBTXS_JD' ."住房补贴-显示(金达)
*&--代码添加 BY HANDYBY 12.06.2017 09:52:17  END

*&--代码添加 BY HANDYBY 29.06.2017 19:20:17  BEGIN
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
    WHEN '9032'.
      L_INTFIELD  = 'GLJTBZ_XS' ."管理津贴标准-显示
    WHEN '9033'.
      L_INTFIELD  = 'ZWJTBZ_XS' ."职位津贴标准-显示
    WHEN '9034'.
      L_INTFIELD  = 'JSJTBZ_XS' ."技术津贴标准-显示
    WHEN '9035'.
      L_INTFIELD  = 'FJJBXZBZ_XS' ."附加基本薪资标准-显示
    WHEN '9036'.
      L_INTFIELD  = 'BTJTBZ_XS' ."补贴津贴标准-显示
*&--代码添加 BY HANDYBY 29.06.2017 19:20:17  END

    WHEN OTHERS.
      CLEAR: L_VALUEFIELD,
             L_INTFIELD.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DOMVALUE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4054   text
*      -->P_<LFS_PC261>_PAYTY  text
*      -->P_LW_OUT_GZHSL  text
*----------------------------------------------------------------------*
FORM GET_DOMVALUE_TEXT USING FU_NAME
                             FU_VALUE
                             FU_TEXT.

  DATA: LW_DD07V TYPE DD07V,
        L_VALUE  TYPE DD07V-DOMVALUE_L,
        L_NAME   TYPE DDOBJNAME.

  CLEAR:LW_DD07V,
        L_VALUE,
        L_NAME.

  L_VALUE = FU_VALUE.
  L_NAME  = FU_NAME.

  "根据domain名字、domain value range值取对应的描述
  CALL FUNCTION 'DDUT_DOMVALUE_TEXT_GET'
    EXPORTING
      NAME          = L_NAME
      VALUE         = L_VALUE
      LANGU         = '1'
*     TEXTS_ONLY    = ' '
    IMPORTING
      DD07V_WA      = LW_DD07V
    EXCEPTIONS
      NOT_FOUND     = 1
      ILLEGAL_INPUT = 2
      OTHERS        = 3.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  FU_TEXT = LW_DD07V-DDTEXT.

ENDFORM.                    " GET_DOMVALUE_TEXT
