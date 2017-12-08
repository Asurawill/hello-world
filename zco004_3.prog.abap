*&---------------------------------------------------------------------*
*& Report  ZCO_R_DLVRD_PRDS_RVTING
*&
*&---------------------------------------------------------------------*
*&
*& 发出商品物料分类账还原
*&---------------------------------------------------------------------*

REPORT ZCO_R_DLVRD_PRDS_RVTING.
*&---------------------------------------------------------------------*
*&-----用到的表
*&---------------------------------------------------------------------*
TABLES: VBRK,ZCOZCYJE,ZCODLVD.
*&---------------------------------------------------------------------*
*&-----类型定义
*&---------------------------------------------------------------------*
TYPES:  BEGIN OF TY_CYZB.    "差异总表
        INCLUDE STRUCTURE ZCOZCYJE.
TYPES:      BLART LIKE BKPF-BLART,  "凭证类型
            BUDAT LIKE BKPF-BUDAT,  "过账日期
            BELNR LIKE BKPF-BELNR,  "凭证编号
            SHKZG LIKE BSEG-SHKZG,  "H借/S贷
            DMBTR LIKE BSEG-DMBTR,  "本位币金额
            PSWSL LIKE BSEG-PSWSL,  "货币
            MENGE LIKE BSEG-MENGE,  "发货数量
            VBELN LIKE BSEG-VBELN,  "开票凭证
            SEL.
TYPES:  END OF TY_CYZB.
*
TYPES:  BEGIN OF TY_KPMX.   "开票明细表
        INCLUDE STRUCTURE ZCOKPMX.
TYPES:      "vbeln LIKE vbrk-vbeln,  "开票凭证
  "rfbsk LIKE vbrk-rfbsk,  "过账状态   C或E
  "fkdat LIKE vbrk-fkdat,  "过账日期
  "vbtyp LIKE vbrk-vbtyp,  "凭证类型
  "vgbel LIKE vbrp-vgbel,  "交货单
  "vgtyp LIKE vbrp-vgtyp,  "先前凭证类型  J
  ABSFE LIKE ZCOKPMX-SHFTF,                   "转出金额的绝对值
  SEL.
TYPES:  END OF TY_KPMX.


TYPES:  BEGIN OF TY_BKPF,
          BUKRS LIKE BKPF-BUKRS,  "公司代码
          BELNR LIKE BKPF-BELNR,  "凭证编号
          GJAHR LIKE BKPF-GJAHR,  "会计年度
          MONAT LIKE BKPF-MONAT,  "期间
          BLART LIKE BKPF-BLART,  "凭证类型
          BUDAT LIKE BKPF-BUDAT,  "过账日期
        END OF TY_BKPF.

TYPES:  BEGIN OF TY_BSEG,
          BUKRS LIKE BSEG-BUKRS,  "公司代码
          BELNR LIKE BSEG-BELNR,  "凭证编号
          GJAHR LIKE BSEG-GJAHR,  "会计年度
          SHKZG LIKE BSEG-SHKZG,  "H借/S贷
          MATNR LIKE BSEG-MATNR,  "物料号
          DMBTR LIKE BSEG-DMBTR,  "本位币金额
          PSWSL LIKE BSEG-PSWSL,  "货币
          MENGE LIKE BSEG-MENGE,  "发货数量
          VBELN LIKE BSEG-VBELN,  "开票凭证
          BUZEI LIKE BSEG-BUZEI,  "行项目号码
        END OF TY_BSEG.

TYPES:  BEGIN OF TY_VBRP,
          "抬头信息
          VBELN LIKE VBRK-VBELN,  "开票凭证
          POSNR LIKE VBRP-POSNR, "开票项目号   "150528 IT02 ADD
          AUBEL LIKE VBRP-AUBEL, "开票的销售订单 "150528 IT02 ADD
          AUPOS LIKE VBRP-AUPOS, "开票的销售订单行项目 "150528 IT02 ADD
          VGBEL LIKE VBRP-VGBEL,  "交货单
          VGTYP LIKE VBRP-VGTYP,  "先前凭证类型  J
          MATNR LIKE VBRP-MATNR,  "物料号
          "  KTGRM LIKE VBRP-KTGRM,  "物料帐户组    150528
          FKIMG LIKE VBRP-FKIMG,  "开票数量
          FKLMG LIKE VBRP-FKLMG, "
          " add by handwlb 20150413
          "  FKLMG LIKE VBRP-FKLMG, "
          "gjahr LIKE vbrk-gjahr,  "会计年度
          "poper LIKE vbrk-poper,  "记账期间
          RFBSK LIKE VBRK-RFBSK,  "过账状态	 C或E
          FKDAT LIKE VBRK-FKDAT,  "过账日期
          BUKRS LIKE VBRK-BUKRS,  "公司代码
          "  VKORG LIKE VBRK-VKORG,  "销售组织         150528
          "  VTWEG LIKE VBRK-VTWEG,  "销售渠道         150528
          VBTYP LIKE VBRK-VBTYP,  "凭证类型
          " KTGRD LIKE VBRK-KTGRD,  "客户帐户组
          "行项目信息

          " add by handwlb 20150413 end
          WADAT LIKE LIKP-WADAT_IST, "交货数量
          SAKN2 LIKE C001-SAKN2,  "成本科目

        END OF TY_VBRP.

TYPES:  BEGIN OF TY_LIKP,
          VBELN LIKE LIKP-VBELN,  "交货单
          WADAT LIKE LIKP-WADAT_IST, "实际交货日期
        END OF TY_LIKP.

TYPES:  BEGIN OF TY_TTDF,
          MATNR LIKE ZCOZCYJE-MATNR,  "物料
          TTDFR LIKE ZCOZCYJE-TTDFR,  "总差异金额
        END OF TY_TTDF.

TYPES:  BEGIN OF TY_JHSL,
          MATNR LIKE ZCOZCYJE-MATNR,  "物料
          ZJHSL LIKE ZCOZCYJE-ZJHSL,  "总交货数量
        END OF TY_JHSL.

TYPES:  BEGIN OF TY_FKMG,
          VBELN LIKE VBRK-VBELN,      "开票凭证
          FKIMG LIKE VBRP-FKIMG,      "开票数量
        END OF TY_FKMG.

TYPES:  BEGIN OF TY_ERRO,
          MATNR    LIKE VBRP-MATNR,      "物料
          FKIMG    LIKE VBRP-FKIMG,      "开票数量
          "add by handwlb
          FKLMG    LIKE VBRP-FKLMG,
          " add end
          DLVMN(6),                   "交货期间
        END OF TY_ERRO.

TYPES:  BEGIN OF TY_SHFT,
          MATNR LIKE VBRP-MATNR,      "物料
          SHFTF LIKE ZCOZCYJE-TTDFR,  "转出金额
          GJAHR LIKE ZCOZCYJE-GJAHR,  "交货年份
          MONAT LIKE ZCOZCYJE-MONAT,  "期间
        END OF TY_SHFT.

TYPES:  BEGIN OF TY_POST,
          AUBEL LIKE VBRP-AUBEL, "销售订单
          AUPOS LIKE VBRP-AUPOS, "销售订单行项目
          SAKN2 LIKE C001-SAKN2,      "成本科目
          MATNR LIKE VBRP-MATNR,      "物料
          MEINS LIKE MARA-MEINS , "单位
          WERKS LIKE MARC-WERKS , "工厂
          KUNNR LIKE VBAK-KUNNR, "客户
          SHFTF LIKE ZCOKPMX-SHFTF,   "转出金额
          FKIMG LIKE VBRP-FKIMG, "开票数量

        END OF TY_POST.
*&---------------------------------------------------------------------*
*&-----内表定义
*&---------------------------------------------------------------------*
DATA: GS_CYZB     TYPE TY_CYZB,
      GT_CYZB     TYPE TABLE OF TY_CYZB,  "差异金额总表

      GS_KPMX     TYPE TY_KPMX,
      GT_KPMX     TYPE TABLE OF TY_KPMX,  "开票明细表

      GS_CYZB_TMP TYPE TY_CYZB,
      GT_CYZB_TMP TYPE TABLE OF TY_CYZB,  "临时差异金额总表

      GS_KPMX_TMP TYPE TY_KPMX,
      GT_KPMX_TMP TYPE TABLE OF TY_KPMX,  "临时开票明细表

      GS_CYZB_PRV TYPE TY_CYZB,
      GT_CYZB_PRV TYPE TABLE OF TY_CYZB,  "非当期的差异金额总表

      LS_KPMX     TYPE TY_KPMX,
      LT_KPMX     TYPE TABLE OF TY_KPMX,

      GS_BKPF     TYPE TY_BKPF,
      GT_BKPF     TYPE TABLE OF TY_BKPF,

      GS_BSEG     TYPE TY_BSEG,
      GT_BSEG     TYPE TABLE OF TY_BSEG,

      GS_TTDF     TYPE TY_TTDF,
      GT_TTDF     TYPE TABLE OF TY_TTDF,

      GS_JHSL     TYPE TY_JHSL,
      GT_JHSL     TYPE TABLE OF TY_JHSL,

      GS_VBRP     TYPE TY_VBRP,
      GT_VBRP     TYPE TABLE OF TY_VBRP,



      GS_LIKP     TYPE TY_LIKP,
      GT_LIKP     TYPE TABLE OF TY_LIKP,

      GS_C001     LIKE C001,
      GT_C001     LIKE TABLE OF C001,

      GS_FKMG     TYPE TY_FKMG,
      GT_FKMG     TYPE TABLE OF TY_FKMG,

      GS_ERRO     TYPE TY_ERRO,
      GT_ERRO     TYPE TABLE OF TY_ERRO,

      GS_SHFT     TYPE TY_SHFT,
      GT_SHFT     TYPE TABLE OF TY_SHFT,

      GS_POST     TYPE TY_POST,
      GT_POST     TYPE TABLE OF TY_POST.
DATA:GT_VBAP   LIKE TABLE OF VBAP WITH HEADER LINE. "150528 ITO2
DATA:GT_VBAK   LIKE TABLE OF VBAK WITH HEADER LINE."销售订单抬头
*过账参数
DATA: "表头
  LS_HEAD       LIKE BAPIACHE09,
  "附加结构-记账码
  LS_ZEXT       LIKE ZACCDOCUEXT,
  "行项--货币
  LS_CURR       LIKE BAPIACCR09,
  LT_CURR       LIKE TABLE OF BAPIACCR09,
  "行项--总账科目
  LS_ACGL       LIKE BAPIACGL09,
  LT_ACGL       LIKE TABLE OF BAPIACGL09,
  "行项--客户
  LS_CSTM       LIKE BAPIACAR09,
  LT_CSTM       LIKE TABLE OF BAPIACAR09,
  "供应商
  LS_LFNR       LIKE BAPIACAP09,
  LT_LFNR       LIKE TABLE OF BAPIACAP09,
  "记账码
  LS_EXTS       LIKE BAPIPAREX,
  LT_EXTS       LIKE TABLE OF BAPIPAREX,
  "返回
  LS_RETURN     LIKE BAPIRET2,
  LT_RETURN     LIKE TABLE OF BAPIRET2,
  "分配1
  LS_CRITERIA   LIKE BAPIACKEC9,
  LT_CRITERIA   LIKE TABLE OF BAPIACKEC9,
  "分配2
  LS_VALUEFIELD LIKE BAPIACKEV9,
  LT_VALUEFIELD LIKE TABLE OF BAPIACKEV9,

  LS_DLVD       LIKE ZCODLVD,
  LT_DLVD       LIKE TABLE OF ZCODLVD,

  GS_DLVD       LIKE ZCODLVD,
  GT_DLVD       LIKE TABLE OF ZCODLVD.

*全局变量
DATA: L_FRSTD        TYPE D,   "当月第一天
      L_LASTD        TYPE D,   "当月最后一天
      L_FIRST_DAY(8),
      L_LAST_DAY(8),
      L_AGAIN,          "重复执行标记
      L_OVER_MONTH.     "跨期标记
*&---------------------------------------------------------------------*
*&-----ALV相关变量定义
*&---------------------------------------------------------------------*
DATA:
  LT_LAYO    TYPE SLIS_LAYOUT_ALV,
  LIT_FCAT   TYPE SLIS_T_FIELDCAT_ALV,
  GIT_EVENTS TYPE SLIS_T_EVENT,
  G_REPID    TYPE SY-REPID VALUE SY-REPID.
*&---------------------------------------------------------------------*
*&-----选择屏幕
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS FOR VBRK-BUKRS NO INTERVALS NO-EXTENSION,
                S_GJAHR FOR VBRK-GJAHR NO INTERVALS NO-EXTENSION,
                S_MONAT FOR ZCOZCYJE-MONAT NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK BLK1.
*
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.
PARAMETERS:C_EXEC RADIOBUTTON GROUP SG1 DEFAULT 'X'.
"PARAMETERS:c_undo RADIOBUTTON GROUP sg1.
PARAMETERS:C_VIEW RADIOBUTTON GROUP SG1.
SELECTION-SCREEN END OF BLOCK BLK2.
*
SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-003.
PARAMETERS:C_CYZB RADIOBUTTON GROUP SG2 DEFAULT 'X'.
PARAMETERS:C_KPMX RADIOBUTTON GROUP SG2.
SELECTION-SCREEN END OF BLOCK BLK3.

AT SELECTION-SCREEN.
  LOOP AT S_BUKRS.
    IF S_BUKRS-LOW IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT 'A_S_WERK'
                 ID 'BUKRS' FIELD S_BUKRS-LOW.
      IF SY-SUBRC <> 0.
        MESSAGE '您不具有该公司的查询权限！' TYPE 'E'.
      ENDIF.
    ENDIF.
    IF S_BUKRS-HIGH IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT 'A_S_WERK'
                 ID 'BUKRS' FIELD S_BUKRS-HIGH.
      IF SY-SUBRC <> 0.
        MESSAGE '您不具有该公司的查询权限！' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDLOOP.

*&---------------------------------------------------------------------*
*&-----事件处理块
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*判断动作
  IF C_EXEC = 'X'.
    PERFORM DATA_GET_FROM_EXEC. "获取数据 - 执行
  ELSE.
    PERFORM DATA_GET_FROM_VIEW. "获取数据 - 查看
  ENDIF.

END-OF-SELECTION.
  PERFORM ALV_CYZB. "ALV展示
*&---------------------------------------------------------------------*
*&-----获取数据-执行
*&---------------------------------------------------------------------*
FORM DATA_GET_FROM_EXEC.
  PERFORM TAB_SEL.  "读取数据
  PERFORM DATA_PROC."数据计算、汇总
ENDFORM.
*&---------------------------------------------------------------------*
*&-----读取数据
*&---------------------------------------------------------------------*
FORM TAB_SEL.
*======================================================================*
*&---------------  总差异金额表   -------------------------------------*
*======================================================================*

  DATA: L_ANSWER  TYPE ANSWER,
        L_FST_DAY TYPE D.
  CLEAR:L_AGAIN.

*检查执行情况
  SELECT *
    INTO TABLE LT_DLVD
    FROM ZCODLVD
    WHERE BUKRS = S_BUKRS-LOW
    AND   GJAHR = S_GJAHR-LOW
    AND   MONAT = S_MONAT-LOW.
*
  IF LT_DLVD IS NOT INITIAL.
*标记重复
    L_AGAIN = 'X'.

    READ TABLE LT_DLVD INTO LS_DLVD INDEX 1.
    IF LS_DLVD-POST = 'X'.
      MESSAGE '该期间已过账，不允许重复执行' TYPE 'E'.
    ENDIF.


    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        TITEL          = TEXT-004
        TEXTLINE1      = TEXT-005
        CANCEL_DISPLAY = SPACE                          "不显示CANCEL按钮
      IMPORTING
        ANSWER         = L_ANSWER.
    IF L_ANSWER = 'N'.
      MESSAGE '操作已取消' TYPE 'E' DISPLAY LIKE 'S'.
    ENDIF.

*如果用户试图重复执行 比已执行过期间 更早的期间，则弹出警告
*如果继续执行，则会覆盖已执行期间 已转出的差异

    SELECT * INTO TABLE GT_DLVD
      FROM ZCODLVD
      WHERE BUKRS = S_BUKRS-LOW.

    LOOP AT GT_DLVD INTO GS_DLVD.
      IF S_GJAHR-LOW < GS_DLVD-GJAHR.
        PERFORM DATA_OVER_WARNING.
      ELSEIF S_GJAHR-LOW = GS_DLVD-GJAHR.
        IF S_MONAT-LOW < GS_DLVD-MONAT.
          PERFORM DATA_OVER_WARNING.
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDIF.
*如果重复执行，先清空上次记录
  IF L_AGAIN = 'X'.

*明细表 只需清空即可
    SELECT *
      INTO TABLE GT_KPMX_TMP
      FROM  ZCOKPMX_TMP
      WHERE GJAHR = S_GJAHR-LOW
      AND   POPER = S_MONAT-LOW
      AND   BUKRS = S_BUKRS-LOW.

    DELETE ZCOKPMX FROM TABLE GT_KPMX_TMP.  "按照临时表清空上次执行记录
    DELETE FROM ZCOKPMX_TMP.                     "清空临时表


*总表，清空上次记录，包括更改的的非当期记录
    SELECT *
      INTO TABLE GT_CYZB_TMP
      FROM ZCOZCYJE_TMP
      WHERE GJAHR = S_GJAHR-LOW
      AND   MONAT = S_MONAT-LOW
      AND   BUKRS = S_BUKRS-LOW.

    DELETE ZCOZCYJE FROM TABLE GT_CYZB_TMP.
    DELETE FROM ZCOZCYJE_TMP.
*还原备份的非当期记录
    SELECT *
      INTO TABLE GT_CYZB_PRV
      FROM ZCOZCYJE_PRV
      " add by robin  20141031 排除工厂
      WHERE BUKRS = S_BUKRS-LOW.
    " add end
    IF GT_CYZB_PRV IS NOT INITIAL.
      MODIFY ZCOZCYJE FROM TABLE GT_CYZB_PRV.  "还原上次备份的非当期记录
      IF SY-SUBRC = 0.
        CLEAR GT_CYZB_PRV.
        DELETE FROM ZCOZCYJE_PRV. "清空备份表
      ENDIF.

    ENDIF.
  ENDIF.


*获取当月最后一天
  CONCATENATE S_GJAHR-LOW S_MONAT-LOW '01' INTO L_FST_DAY.



  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = L_FST_DAY
    IMPORTING
      LAST_DAY_OF_MONTH = L_LASTD
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.


*根据输入条件获取会计核算抬头
  SELECT  BUKRS   "公司代码
          BELNR   "凭证编号
          GJAHR   "会计年度
          MONAT   "期间
          BLART   "凭证类型
          BUDAT   "过账日期
    INTO TABLE GT_BKPF
    FROM BKPF
    WHERE  BUKRS = S_BUKRS-LOW       "输入条件
    AND    GJAHR = S_GJAHR-LOW
    AND    MONAT = S_MONAT-LOW
    AND   ( BLART = 'ML' OR BLART = 'WL' OR BLART = 'WA')   "隐含条件
    AND    BUDAT BETWEEN L_FST_DAY AND L_LASTD. "过账期间为当期


*根据抬头获取行
  IF GT_BKPF IS NOT INITIAL.
    SELECT  BUKRS   "公司代码
            BELNR   "凭证编号
            GJAHR   "会计年度
            SHKZG   "H借/S贷
            MATNR   "物料号
            DMBTR   "本位币金额
            PSWSL   "货币
            MENGE   "发货数量
            VBELN   "开票凭证
            BUZEI   "行项目号码
    INTO TABLE GT_BSEG
    FROM BSEG
    FOR ALL ENTRIES IN GT_BKPF
    WHERE GJAHR =  GT_BKPF-GJAHR
    AND   BUKRS =  GT_BKPF-BUKRS
    AND   BELNR =  GT_BKPF-BELNR
    AND  ( HKONT = '1406010101'
*&--代码添加 BY HANDYBY 26.07.2017 14:35:12  BEGIN
      OR HKONT = '1406010102'
*&--代码添加 BY HANDYBY 26.07.2017 14:35:12  END
    OR HKONT BETWEEN '6001*' AND '6051*')
    AND   ( SHKZG =  'H' OR SHKZG = 'S').


  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&-------------------------覆盖警告------------------------------------*
FORM DATA_OVER_WARNING.
  DATA: DATA_OVER_ANSWER TYPE ANSWER.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      TITEL          = TEXT-006
      TEXTLINE1      = TEXT-007
      CANCEL_DISPLAY = SPACE                          "不显示CANCEL按钮
    IMPORTING
      ANSWER         = DATA_OVER_ANSWER.
  IF DATA_OVER_ANSWER = 'N'.
    MESSAGE '操作已取消' TYPE 'E' DISPLAY LIKE 'S'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&----- 数据计算、汇总
*&---------------------------------------------------------------------*
FORM DATA_PROC.
*============================总表======================================*
  DATA: L_ZJHSL      LIKE ZCOZCYJE-ZJHSL,
        L_FKIMG      LIKE VBRP-FKIMG,
        L_FKLMG      LIKE VBRP-FKLMG,
        L_UDFFR      LIKE ZCOZCYJE-UDFFR,
        L_SHFTF      LIKE ZCOKPMX-SHFTF,
        L_SUM_SFTDF  LIKE ZCOZCYJE-TTDFR,
        L_DIFFER     LIKE ZCOZCYJE-SFTDF,
        L_TEMP_SHFTF LIKE ZCOKPMX-SHFTF,
        L_HS_FLAG.


*方便调试时查看物料对应的相关数据
  SORT GT_BSEG BY BELNR MATNR.


*合并抬头，取凭证类型
  LOOP AT GT_BSEG INTO GS_BSEG.
    CLEAR GS_CYZB.
    MOVE-CORRESPONDING GS_BSEG TO GS_CYZB.

    LOOP AT GT_BKPF INTO GS_BKPF WHERE BELNR = GS_BSEG-BELNR
                                  AND   BUKRS = GS_BSEG-BUKRS
                                  AND   GJAHR = GS_BSEG-GJAHR.
      CLEAR:GS_CYZB-BLART,GS_CYZB-BUDAT,GS_CYZB-MONAT.
      GS_CYZB-BLART = GS_BKPF-BLART.   "凭证类型
      GS_CYZB-BUDAT = GS_BKPF-BUDAT.   "过账日期
      GS_CYZB-MONAT = GS_BKPF-MONAT.   "期间
      APPEND GS_CYZB TO GT_CYZB.
    ENDLOOP.
  ENDLOOP.

  " BREAK-POINT.

*转换借贷正负值，按物料累计金额、数量
  LOOP AT GT_CYZB INTO GS_CYZB.
    IF GS_CYZB-SHKZG = 'H'.   "借贷标志为‘贷’时，金额为负
      GS_CYZB-DMBTR = 0 - GS_CYZB-DMBTR.
      GS_CYZB-MENGE = 0 - GS_CYZB-MENGE.    "交货数量为负
    ENDIF.

    IF GS_CYZB-BLART = 'ML' AND GS_CYZB-BUDAT = L_LASTD. "过账日期为当月最后一天，汇总金额
      CLEAR GS_TTDF.
      GS_TTDF-MATNR = GS_CYZB-MATNR.
      GS_TTDF-TTDFR = GS_CYZB-DMBTR.
      COLLECT GS_TTDF INTO GT_TTDF.
    ELSE.                    "汇总数量
      CLEAR GS_JHSL.
      GS_JHSL-MATNR = GS_CYZB-MATNR.
      GS_JHSL-ZJHSL = GS_CYZB-MENGE.
      COLLECT GS_JHSL INTO GT_JHSL.
    ENDIF.

    MODIFY GT_CYZB FROM GS_CYZB.

  ENDLOOP.

*汇总到总表输出
  LOOP AT GT_CYZB INTO GS_CYZB.
    GS_CYZB-MANDT = SY-MANDT.
    CLEAR: GS_CYZB-DMBTR,GS_CYZB-MENGE,GS_KPMX. "清空金额、数量以便删除重复行

    READ TABLE GT_TTDF INTO GS_TTDF WITH KEY MATNR = GS_CYZB-MATNR.
    IF SY-SUBRC = 0.
      GS_CYZB-TTDFR = GS_TTDF-TTDFR.  "总差异金额
    ENDIF.

    READ TABLE GT_JHSL INTO GS_JHSL WITH KEY MATNR = GS_CYZB-MATNR.
    IF SY-SUBRC = 0.
      GS_CYZB-ZJHSL = GS_JHSL-ZJHSL.  "总交货数量
    ENDIF.

    IF GS_CYZB-ZJHSL NE 0.
      GS_CYZB-UDFFR = GS_CYZB-TTDFR / GS_CYZB-ZJHSL.
    ENDIF.

    MODIFY GT_CYZB FROM GS_CYZB.

  ENDLOOP.

  SORT GT_CYZB BY GJAHR MONAT BUKRS MATNR.
  DELETE ADJACENT DUPLICATES FROM GT_CYZB COMPARING GJAHR MONAT BUKRS MATNR.



*============================明细表====================================*

*根据年度会计计算日期范围
*第一天
  CONCATENATE S_GJAHR-LOW S_MONAT-LOW '01' INTO L_FIRST_DAY.
  L_FRSTD = L_FIRST_DAY.
*最后一天
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = L_FRSTD
    IMPORTING
      LAST_DAY_OF_MONTH = L_LASTD
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.
*取发票抬头、行
*  SELECT
*          "vbrk~gjahr   "会计年度
*          "vbrk~poper   "记账期间
*          VBRK~RFBSK   "过账状态   C或E
*          VBRK~FKDAT   "过账日期
*          VBRK~BUKRS   "公司代码
*         " VBRK~VKORG   "销售组织
*        "  VBRK~VTWEG   "销售渠道
*       "   VBRK~VBTYP   "凭证类型
*          "VBRK~KTGRD   "客户帐户组
*         VBRK~VBELN   "开票凭证
*      "    VBRP~POSNR  "开票项目号  150528
*          VBRP~VGBEL   "交货单
*          VBRP~VGTYP   "先前凭证类型  J
*          VBRP~MATNR   "物料号
*          "VBRP~KTGRM   "物料帐户组
*          VBRP~FKIMG   "开票数量
*         "add by handwlb 20150410
*          VBRP~FKLMG"库存单位的开票数量
*         "add end
*          "add by LYDIT02 150528 begin
*        "  VBRP~AUBEL     "销售订单
*        "  VBRP~AUPOS     "销售订单行项目
*           "add 150528
*    INTO TABLE GT_VBRP
*    FROM  VBRK JOIN VBRP
*    ON    VBRK~VBELN = VBRP~VBELN
*    WHERE ( VBRK~RFBSK = 'C' OR VBRK~RFBSK = 'E')
*"**************add by wlb 剔除一种业务类型，VBRK-FKTYP不等于I 20141031
**    and vbrk~fktyp <> 'I'
*"******************
*          " add by robin  20141031 排除工厂
*    AND BUKRS = S_BUKRS-LOW
*      " add end
*    AND   VBRP~VGTYP IN ('J','T')
*    AND   VBRK~FKDAT BETWEEN L_FRSTD AND L_LASTD    "开票时间有待限定************
*    AND   VBRP~WERKS IN ('1100','1000').
  SELECT
           VBRP~VBELN   "开票凭证
          VBRP~POSNR  "开票项目号  150528
          VBRP~AUBEL     "销售订单
          VBRP~AUPOS     "销售订单行项目

          VBRP~VGBEL   "交货单
          VBRP~VGTYP   "先前凭证类型  J
          VBRP~MATNR   "物料号
          VBRP~FKIMG   "开票数量
          VBRP~FKLMG"库存单位的开票数量
           VBRK~RFBSK   "过账状态   C或E

          VBRK~FKDAT   "过账日期
          VBRK~BUKRS   "公司代码
           VBRK~VBTYP "凭证类型
    INTO TABLE GT_VBRP
    FROM  VBRP JOIN VBRK
    ON    VBRP~VBELN = VBRK~VBELN
    WHERE ( VBRK~RFBSK = 'C' OR VBRK~RFBSK = 'E')
     AND BUKRS = S_BUKRS-LOW
    AND   VBRP~VGTYP IN ('J','T')
    AND   VBRK~FKDAT BETWEEN L_FRSTD AND L_LASTD  .   "开票时间有待限定************
  " AND   VBRP~WERKS IN ('1100','1000').
  "BREAK-POINT.
  SORT GT_VBRP BY FKDAT BUKRS AUBEL AUPOS.
*根据交货单取交货数量
  IF GT_VBRP IS NOT INITIAL.
    SELECT VBELN WADAT_IST
      INTO TABLE GT_LIKP
      FROM LIKP
      FOR ALL ENTRIES IN GT_VBRP
      WHERE VBELN = GT_VBRP-VGBEL.
*取成本科目
*    SELECT *
*      INTO TABLE GT_C001
*      FROM C001
*      FOR ALL ENTRIES IN GT_VBRP
*      WHERE KAPPL  = 'V'                "150528 modify to  统一科目为"6401010101"
*      AND   KSCHL  = 'KOFI'
*      AND   KTOPL  = '1000'
*      AND   VKORG  = GT_VBRP-VKORG
*      "AND   vtweg  = gt_kpmx-vtweg
*      AND   KTGRD  = GT_VBRP-KTGRD
*      AND   KTGRM = GT_VBRP-KTGRM
*      AND   KVSL1  = 'ZCO'.
    " add it02 150528
    "取 销售订单 和行项目信息
    SELECT VBELN POSNR WERKS MEINS INTO CORRESPONDING FIELDS OF TABLE GT_VBAP
      FROM VBAP
      FOR ALL ENTRIES IN GT_VBRP
      WHERE VBELN = GT_VBRP-AUBEL
      AND   POSNR = GT_VBRP-AUPOS.

                                                            "150528
    "add 当前销售订单抬头读取 客户信息 IT02  150619 begin
    SELECT VBELN KUNNR INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
      FROM VBAK
      FOR ALL ENTRIES IN GT_VBRP
      WHERE VBELN = GT_VBRP-AUBEL .
    "add 当前销售订单抬头读取 客户信息 IT02  150619 end
  ENDIF.
  LOOP AT GT_VBRP INTO GS_VBRP.

    "add 150528 删除 vbap 不存在的销售订单 、行项目begin
    READ TABLE GT_VBAP WITH KEY VBELN = GS_VBRP-AUBEL POSNR = GS_VBRP-AUPOS .
    IF SY-SUBRC <> 0 .
      DELETE GT_VBRP WHERE  AUBEL = GS_VBRP-AUBEL AND  AUPOS = GS_VBRP-AUPOS .
    ENDIF.

    "add 150528   end


  ENDLOOP.

  LOOP AT GT_VBRP INTO GS_VBRP.


    CLEAR GS_KPMX.
    "*发票信息
    "    MOVE-CORRESPONDING gs_vbrp to gs_kpmx.
*转换数量正负值
    IF GS_VBRP-VBTYP = 'N' OR GS_VBRP-VBTYP = 'O' OR GS_VBRP-VBTYP = '6'.
      GS_VBRP-FKIMG = 0 - GS_VBRP-FKIMG. "若VBRK-VBTYP=N,O,6，则将此数量改为负数
      "add by handwlb
      GS_VBRP-FKLMG = 0 - GS_VBRP-FKLMG.
      "end
    ENDIF.
*交货期间
    READ TABLE GT_LIKP INTO GS_LIKP WITH KEY VBELN = GS_VBRP-VGBEL.
    IF SY-SUBRC = 0.
      GS_VBRP-WADAT = GS_LIKP-WADAT.    "交货期间
    ENDIF.
*成本科目
*    READ TABLE GT_C001 INTO GS_C001 WITH KEY VKORG = GS_VBRP-VKORG
*                                             KTGRD = GS_VBRP-KTGRD
*                                             KTGRM = GS_VBRP-KTGRM.       "150528 替换为 ”6401010101
*    IF SY-SUBRC = 0.
*      GS_VBRP-SAKN2 = GS_C001-SAKN2.    "成本科目
*    ENDIF.
    "IT02 MODIFY  REPLACE '6401010101' WITH '6401020101' WHEN KUNNR = '1000' OR GS_VBAK-KUNNR = '1100' .150619 begin
*    READ TABLE GT_VBAK  WITH KEY VBELN = GS_VBRP-AUBEL.
*    IF SY-SUBRC = 0.
*       IF GT_VBAK-KUNNR = '0000001000' OR GT_VBAK-KUNNR = '0000001100' .
*         GS_VBRP-SAKN2 = '6401020101'. "主营业务成本-关联方
*        ELSE.
*          GS_VBRP-SAKN2 = '6401010101'."主营业务成本-非关联方
*       ENDIF.
*     ELSE.
*       GS_VBRP-SAKN2 = '6401010101'."主营业务成本-非关联方
*    ENDIF.

    GS_VBRP-SAKN2 = '6401010101'."主营业务成本-非关联方  " 记账科目固定为 ：6401010101  modified by it02  at 150928
    "IT02 MODIFY  REPLACE '6401010101' WITH '6401020101' WHEN KUNNR = '1000' OR GS_VBAK-KUNNR = '1100' .150619 end
    "   GS_VBRP-SAKN2 = '6401010101'.
    MODIFY GT_VBRP FROM GS_VBRP.
*按key汇总
    GS_KPMX-GJAHR = GS_VBRP-FKDAT+0(4).    "会计年度
    GS_KPMX-POPER = GS_VBRP-FKDAT+4(2).    "期间

    GS_KPMX-BUKRS = GS_VBRP-BUKRS.    "公司代码
*    GS_KPMX-VKORG = GS_VBRP-VKORG.    "销售组织
*    GS_KPMX-VTWEG = GS_VBRP-VTWEG.    "销售渠道      "150528  更改为 销售订单、行项目分组统计数量
*    GS_KPMX-KTGRD = GS_VBRP-KTGRD.    "客户账户组
*    GS_KPMX-KTGRM = GS_VBRP-KTGRM.    "物料账户组
    GS_KPMX-AUBEL = GS_VBRP-AUBEL ."销售订单
    GS_KPMX-AUPOS = GS_VBRP-AUPOS."销售订单行项目
    GS_KPMX-SAKN2 = GS_VBRP-SAKN2.    "成本科目
    GS_KPMX-MATNR = GS_VBRP-MATNR.    "物料编号
    GS_KPMX-DLVDT = GS_VBRP-WADAT+0(6).    "发货期间
    GS_KPMX-FKIMG = GS_VBRP-FKIMG.    "开票数量
    "add by handwlb
    GS_KPMX-FKLMG = GS_VBRP-FKLMG.
    "add end
    COLLECT GS_KPMX INTO GT_KPMX.
  ENDLOOP.
  "BREAK-POINT.

*&---------------------------------------------------------------------*
*&   按物料排序，汇总开票数量，若汇总数量大于总表中总交货数量，
*&   则设置此物料本期所有行“错误标示”为X，不做计算
*&---------------------------------------------------------------------*
  SORT GT_KPMX BY MATNR.
*按物料计算开票数量
  LOOP AT GT_KPMX INTO GS_KPMX.

    "跨期判断
    IF GS_KPMX-DLVDT+0(4) NE S_GJAHR-LOW OR GS_KPMX-DLVDT+4(2) NE S_MONAT-LOW.
      L_OVER_MONTH = 'X'.
    ENDIF.
    IF GS_KPMX-FKIMG = 0.
      DELETE GT_KPMX.
      CONTINUE.
    ENDIF.
    CLEAR: GS_ERRO.
    GS_ERRO-MATNR = GS_KPMX-MATNR.
    GS_ERRO-FKIMG = GS_KPMX-FKIMG.
    GS_ERRO-FKLMG = GS_KPMX-FKLMG.
    GS_ERRO-DLVMN = GS_KPMX-DLVDT+0(6).
    COLLECT GS_ERRO INTO GT_ERRO.
  ENDLOOP.



*跨期
  IF L_OVER_MONTH = 'X'.
    "找出符合交货期间的非当期总差异记录
    SELECT *
      INTO TABLE GT_CYZB_PRV
      FROM ZCOZCYJE
      FOR ALL ENTRIES IN GT_KPMX
      WHERE GJAHR = GT_KPMX-DLVDT+0(4)
      AND   MONAT = GT_KPMX-DLVDT+4(2)
      AND   BUKRS = GT_KPMX-BUKRS.

    IF GT_CYZB_PRV IS NOT INITIAL.
      DELETE FROM ZCOZCYJE_PRV.
      MODIFY ZCOZCYJE_PRV FROM TABLE GT_CYZB_PRV.  "备份非当期记录
    ENDIF.
  ENDIF.

  APPEND LINES OF GT_CYZB_PRV TO GT_CYZB.
  SORT GT_CYZB BY GJAHR MONAT BUKRS MATNR.
  DELETE ADJACENT DUPLICATES FROM GT_CYZB COMPARING GJAHR MONAT BUKRS MATNR.

*判断错误标记
  LOOP AT GT_KPMX INTO GS_KPMX.
*初始化
    CLEAR: L_FKIMG,
           L_ZJHSL,
           L_UDFFR,
           GS_KPMX-ERR.


*读取累计开票数量
    READ TABLE GT_ERRO INTO GS_ERRO WITH KEY MATNR = GS_KPMX-MATNR DLVMN = GS_KPMX-DLVDT+0(6).
    IF SY-SUBRC = 0.
      L_FKIMG = GS_ERRO-FKIMG.  "按物料汇总的发票数量
      L_FKLMG = GS_ERRO-FKLMG.
    ENDIF.
*读取差异总表中的总交货数量
    READ TABLE GT_CYZB INTO GS_CYZB WITH KEY GJAHR = GS_KPMX-DLVDT+0(4)
                                             MONAT = GS_KPMX-DLVDT+4(2)
                                             BUKRS = GS_KPMX-BUKRS
                                             MATNR = GS_KPMX-MATNR.
    IF SY-SUBRC = 0.
      L_ZJHSL = GS_CYZB-ZJHSL.  "总交货数量
      L_UDFFR = GS_CYZB-UDFFR.  "单位差异金额
*进行对比
*      IF l_fklmg GT l_zjhsl.
*        gs_kpmx-err = 'X'.        "更新错误标志
*      ELSE.
      "在总表中取相应期间，应期间物料的单位差异金额，
      "乘以数量，更新每行的“转出金额(2位小数）”
      "fkimg 替换为flimg change by handwlb 20150413
      "gs_kpmx-shftf = l_udffr * gs_kpmx-fkimg. "转出金额
      GS_KPMX-SHFTF = L_UDFFR * GS_KPMX-FKLMG.
      "

*      ENDIF.
    ENDIF.
*更新内表
    MODIFY GT_KPMX FROM GS_KPMX.
  ENDLOOP.

*&---------------------------------------------------------------------*
*&   更新总表已转出差异
*&---------------------------------------------------------------------*

*按物料汇总差异金额
  LOOP AT GT_KPMX INTO GS_KPMX WHERE ERR NE 'X'.  "有错误标志的行不参与计算
    CLEAR GS_SHFT.
*计算转出金额绝对值
    IF GS_KPMX-SHFTF < 0.
      GS_KPMX-ABSFE = 0 - GS_KPMX-SHFTF.
    ELSE.
      GS_KPMX-ABSFE =  GS_KPMX-SHFTF.
    ENDIF.
    MODIFY GT_KPMX FROM GS_KPMX.
*累计转出金额
    GS_SHFT-MATNR = GS_KPMX-MATNR.
    GS_SHFT-SHFTF = GS_KPMX-SHFTF.
    GS_SHFT-GJAHR = GS_KPMX-DLVDT+0(4).
    GS_SHFT-MONAT = GS_KPMX-DLVDT+4(2).
    COLLECT GS_SHFT INTO GT_SHFT.
  ENDLOOP.



*按转出金额绝对值降序
  SORT GT_KPMX BY GJAHR DLVDT MATNR ABSFE DESCENDING.
*更新已转出差异
  LOOP AT GT_CYZB INTO GS_CYZB.

*初始化
    CLEAR: L_SHFTF,L_SUM_SFTDF,L_DIFFER.

*读取转出金额
    READ TABLE  GT_KPMX INTO GS_KPMX WITH KEY DLVDT+0(4) = GS_CYZB-GJAHR
                                              DLVDT+4(2) = GS_CYZB-MONAT
                                              BUKRS      = GS_CYZB-BUKRS
                                              MATNR      = GS_CYZB-MATNR
                                              ERR        = ''.
    IF SY-SUBRC = 0.
      READ TABLE GT_SHFT INTO GS_SHFT WITH KEY MATNR = GS_CYZB-MATNR
                                               GJAHR = GS_CYZB-GJAHR
                                               MONAT = GS_CYZB-MONAT.
      IF SY-SUBRC = 0.
        L_SUM_SFTDF = GS_CYZB-SFTDF + GS_SHFT-SHFTF.      "累计已转出金额
      ENDIF.
*比较总差异金额
      IF ABS( L_SUM_SFTDF ) LE ABS( GS_CYZB-TTDFR ).
        GS_CYZB-SFTDF = L_SUM_SFTDF.
      ELSE."选最大数量行吸收金额
        L_DIFFER = L_SUM_SFTDF - GS_CYZB-TTDFR.
        LOOP AT GT_KPMX INTO GS_KPMX WHERE DLVDT+0(4) = GS_CYZB-GJAHR
                                     AND  DLVDT+4(2) = GS_CYZB-MONAT
                                     AND  BUKRS      = GS_CYZB-BUKRS
                                     AND  MATNR      = GS_CYZB-MATNR
                                     AND  ERR        = ''.

          GS_KPMX-SHFTF = GS_KPMX-SHFTF - L_DIFFER.
          MODIFY GT_KPMX FROM GS_KPMX.
          EXIT.
        ENDLOOP.
        GS_CYZB-SFTDF = GS_CYZB-TTDFR.
      ENDIF.

      "        LOOP AT gt_kpmx INTO gs_kpmx WHERE  dlvdt+0(4) = gs_cyzb-gjahr
      "                                     AND    dlvdt+4(2) = gs_cyzb-monat
      "                                     AND    bukrs      = gs_cyzb-bukrs
      "                                     AND    matnr      = gs_cyzb-matnr
      "                                     AND    err        = ''.
      "          l_temp_shftf = gs_kpmx-shftf - l_differ.  "由最大值吸收差异
      "         IF l_temp_shftf GE 0.
      "            gs_kpmx-shftf = l_temp_shftf.           "更新转出金额
      "            gs_cyzb-sftdf = gs_kpmx-shftf.
      "            MODIFY gt_kpmx FROM gs_kpmx.
      "
      "            EXIT.
      "          ELSE.
      "            "gs_kpmx-shftf = 0.                      "最大值全部吸收
      "            "修改逻辑：若一次性吸收值为全部转出金额，则按总差异金额作为转出
      "            gs_kpmx-shftf = gs_cyzb-ttdfr - l_sum_sftdf.
      "            gs_cyzb-sftdf = gs_cyzb-ttdfr.
      "
      "
      "           MODIFY gt_kpmx FROM gs_kpmx.
      "           "l_differ = l_differ - gs_kpmx-shftf.    "更新差异值

      "            EXIT.
      "          ENDIF.
      "       ENDLOOP.



      MODIFY GT_CYZB FROM GS_CYZB. "更新已转出差异
    ENDIF.
  ENDLOOP.

  "BREAK-POINT.
*&---------------------------------------------------------------------*
*&----- 写入自建表
*&---------------------------------------------------------------------*
  DATA: L_ZCYB_STA,
        L_KPMX_STA.

  MODIFY ZCOZCYJE_TMP FROM TABLE GT_CYZB.     "写入临时表
  IF SY-SUBRC = 0.
  ENDIF.

  MODIFY ZCOKPMX_TMP FROM TABLE GT_KPMX.   "临时表
  IF SY-SUBRC = 0.
  ENDIF.



  MODIFY ZCOZCYJE FROM TABLE GT_CYZB.      "总差异金额表
  IF SY-SUBRC = 0.
    L_ZCYB_STA = 'X'.
  ENDIF.

  MODIFY ZCOKPMX  FROM TABLE GT_KPMX.      "开票明细

  IF SY-SUBRC = 0.
    L_KPMX_STA = 'X'.
  ENDIF.

  IF C_CYZB = 'X'.
    IF L_ZCYB_STA = 'X' AND L_KPMX_STA = 'X'.
      MESSAGE '写入自建表成功，同时生成开票明细表' TYPE 'S'.
    ENDIF.
  ELSE.
    IF L_ZCYB_STA = 'X' AND L_KPMX_STA = 'X'.
      MESSAGE '写入自建表成功，同时更新总差异明细表' TYPE 'S'.
    ENDIF.
  ENDIF.

*记录过账期间
  CLEAR: LS_DLVD,LT_DLVD.
  LS_DLVD-BUKRS = S_BUKRS-LOW.
  LS_DLVD-GJAHR = S_GJAHR-LOW.
  LS_DLVD-MONAT = S_MONAT-LOW.
  LS_DLVD-UNAME = SY-UNAME.
  LS_DLVD-ERDAT = SY-DATUM.
  LS_DLVD-EXETS = SY-UZEIT.
  APPEND LS_DLVD TO LT_DLVD.
  MODIFY ZCODLVD FROM TABLE LT_DLVD.



*不显示非当期总表记录
  LOOP AT GT_CYZB INTO GS_CYZB.
    IF GS_CYZB-GJAHR NE S_GJAHR-LOW OR GS_CYZB-MONAT NE S_MONAT-LOW.
      DELETE GT_CYZB.
      CONTINUE.
    ENDIF.

  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*&----- ALV展示
*&---------------------------------------------------------------------*
FORM ALV_CYZB.
  PERFORM SETFIED.      "字段
  PERFORM SETLAYOUT.    "布局
  PERFORM ALV_DISPLAY.  "展示
ENDFORM.
*&---------------------------------------------------------------------*
*&----- 字段设置
*&---------------------------------------------------------------------*
FORM SETFIED .


* 字段设置
  DATA LS_FCAT TYPE SLIS_FIELDCAT_ALV.

  IF C_CYZB = 'X'.  "显示总差异金额表

    "会计年度
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'GJAHR'.
    LS_FCAT-SELTEXT_L  = '会计年度'.
    APPEND LS_FCAT TO LIT_FCAT.

    "期间
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'MONAT'.
    LS_FCAT-SELTEXT_L  = '期间'.
    APPEND LS_FCAT TO LIT_FCAT.

    "公司代码
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'BUKRS'.
    LS_FCAT-SELTEXT_L  = '公司代码'.
    APPEND LS_FCAT TO LIT_FCAT.

    "物料
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'MATNR'.
    LS_FCAT-SELTEXT_L  = '物料'.
    APPEND LS_FCAT TO LIT_FCAT.

    "总交货数量
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'ZJHSL'.
    LS_FCAT-SELTEXT_L  = '总交货数量'.
    APPEND LS_FCAT TO LIT_FCAT.

    "总差异金额
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'TTDFR'.
    LS_FCAT-SELTEXT_L  = '总差异金额'.
    APPEND LS_FCAT TO LIT_FCAT.

    "单位差异金额
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'UDFFR'.
    LS_FCAT-SELTEXT_L  = '单位差异金额'.
    APPEND LS_FCAT TO LIT_FCAT.

    "已转出差异
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'SFTDF'.
    LS_FCAT-SELTEXT_L  = '已转出差异'.
    APPEND LS_FCAT TO LIT_FCAT.

  ELSE.       "显示开票明细表

    "会计年度
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'GJAHR'.
    LS_FCAT-SELTEXT_L  = '会计年度'.
    APPEND LS_FCAT TO LIT_FCAT.

    "期间
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = ' POPER'.
    LS_FCAT-SELTEXT_L  = '开票期间'.
    APPEND LS_FCAT TO LIT_FCAT.

    "公司代码
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'BUKRS'.
    LS_FCAT-SELTEXT_L  = '公司代码'.
    APPEND LS_FCAT TO LIT_FCAT.

*    "销售组织
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME = 'VKORG'.
*    LS_FCAT-SELTEXT_L  = '销售组织'.
*    APPEND LS_FCAT TO LIT_FCAT.
*
*
*    "分销渠道
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME = 'VTWEG'.
*    LS_FCAT-SELTEXT_L  = '分销渠道'.
*    APPEND LS_FCAT TO LIT_FCAT.
*
*    "客户账户组
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME = 'KTGRD'.
*    LS_FCAT-SELTEXT_L  = '客户账户组'.
*    APPEND LS_FCAT TO LIT_FCAT.
*
*    "物料账户组
*    CLEAR LS_FCAT.
*    LS_FCAT-FIELDNAME = 'KTGRM'.
*    LS_FCAT-SELTEXT_L  = '物料账户组'.
*    APPEND LS_FCAT TO LIT_FCAT.
    "销售订单
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'AUBEL'.
    LS_FCAT-SELTEXT_L  = '销售订单'.
    APPEND LS_FCAT TO LIT_FCAT.
    "销售订单行项目
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'AUPOS'.
    LS_FCAT-SELTEXT_L  = '销售订单行项目'.
    APPEND LS_FCAT TO LIT_FCAT.
    "成本科目
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'SAKN2'.
    LS_FCAT-SELTEXT_L  = '销售成本科目'.
    APPEND LS_FCAT TO LIT_FCAT.

    "物料
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'MATNR'.
    LS_FCAT-SELTEXT_L  = '物料号'.
    APPEND LS_FCAT TO LIT_FCAT.

    "交货期间
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'DLVDT'.
    LS_FCAT-SELTEXT_L  = '交货期间'.
    APPEND LS_FCAT TO LIT_FCAT.

    "开票数量
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'FKIMG'.
    LS_FCAT-SELTEXT_L  = '开票数量'.
    APPEND LS_FCAT TO LIT_FCAT.

    "转出金额
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'SHFTF'.
    LS_FCAT-SELTEXT_L  = '转出金额'.
    APPEND LS_FCAT TO LIT_FCAT.

    "错误标记
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'ERR'.
    LS_FCAT-SELTEXT_L  = '错误标记'.
    APPEND LS_FCAT TO LIT_FCAT.

    "过账凭证
    CLEAR LS_FCAT.
    LS_FCAT-FIELDNAME = 'BELNR'.
    LS_FCAT-SELTEXT_L = '过账凭证'.
    APPEND LS_FCAT TO LIT_FCAT.
  ENDIF.
*
ENDFORM.




*&---------------------------------------------------------------------*
*&-----获取数据-查看
*&---------------------------------------------------------------------*
FORM DATA_GET_FROM_VIEW.
  IF C_CYZB = 'X'.
    SELECT *
      INTO TABLE GT_CYZB
      FROM ZCOZCYJE
      WHERE GJAHR = S_GJAHR-LOW
      AND   MONAT = S_MONAT-LOW
      AND   BUKRS = S_BUKRS-LOW.
  ELSE.
*根据年度会计计算日期范围
*第一天
    CONCATENATE S_GJAHR-LOW S_MONAT-LOW '01' INTO L_FIRST_DAY.
    L_FRSTD = L_FIRST_DAY.
*最后一天
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = L_FRSTD
      IMPORTING
        LAST_DAY_OF_MONTH = L_LASTD
      EXCEPTIONS
        DAY_IN_NO_DATE    = 1
        OTHERS            = 2.
**取发票抬头、行
*    SELECT  VBRK~VBELN   "开票凭证
*            "vbrk~gjahr   "会计年度
*            "vbrk~poper   "记账期间
*            VBRK~RFBSK   "过账状态   C或E
*            VBRK~FKDAT   "过账日期
*            VBRK~BUKRS   "公司代码
*            VBRK~VKORG   "销售组织
*            VBRK~VTWEG   "销售渠道
*            VBRK~VBTYP   "凭证类型
*            VBRK~KTGRD   "客户帐户组
*            VBRP~POSNR   "开票行项目号  "150528 ITO2 ADD
*            VBRP~VGBEL   "交货单
*            VBRP~VGTYP   "先前凭证类型  J
*            VBRP~MATNR   "物料号
*            VBRP~KTGRM   "物料帐户组
*            VBRP~FKIMG   "开票数量
*
*            VBRP~FKLMG"add by handwlb
*
*      INTO TABLE GT_VBRP
*      FROM  VBRK JOIN VBRP
*      ON    VBRK~VBELN = VBRP~VBELN
*      WHERE ( VBRK~RFBSK = 'C' OR VBRK~RFBSK = 'E')
*      AND   VBRP~VGTYP IN ('J','T')
    SELECT
          VBRP~VBELN   "开票凭证
         VBRP~POSNR  "开票项目号  150528
         VBRP~AUBEL     "销售订单
         VBRP~AUPOS     "销售订单行项目

         VBRP~VGBEL   "交货单
         VBRP~VGTYP   "先前凭证类型  J
         VBRP~MATNR   "物料号
         VBRP~FKIMG   "开票数量
         VBRP~FKLMG"库存单位的开票数量
          VBRK~RFBSK   "过账状态   C或E

         VBRK~FKDAT   "过账日期
         VBRK~BUKRS   "公司代码
          VBRK~VBTYP "凭证类型
   INTO TABLE GT_VBRP
   FROM  VBRP JOIN VBRK
      ON    VBRK~VBELN = VBRP~VBELN
    WHERE ( VBRK~RFBSK = 'C' OR VBRK~RFBSK = 'E')
     AND   VBRP~VGTYP IN ('J','T')

"**************add by wlb 剔除一种业务类型，VBRK-FKTYP不等于I 20141031
*    and vbrk~fktyp <> 'I'
"******************
     AND   VBRK~FKDAT BETWEEN L_FRSTD AND L_LASTD   "开票时间有待限定************
     AND   VBRK~BUKRS = S_BUKRS-LOW .
    "    AND   VBRP~WERKS IN ('1100','1000').

    IF GT_VBRP IS NOT INITIAL.
      SELECT *
        INTO TABLE GT_KPMX
        FROM ZCOKPMX
        FOR ALL ENTRIES IN GT_VBRP
        WHERE BUKRS = GT_VBRP-BUKRS
        AND   GJAHR = GT_VBRP-FKDAT+0(4)
        AND   POPER = GT_VBRP-FKDAT+4(2).
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&----- ALV布局
*&---------------------------------------------------------------------*
FORM SETLAYOUT .
  LT_LAYO-COLWIDTH_OPTIMIZE = 'X'.         "自动缩进
  LT_LAYO-ZEBRA = 'X'.                     "斑马线
  LT_LAYO-INFO_FIELDNAME = 'ROW_COLOR'.    "OUTTAB
  LT_LAYO-BOX_FIELDNAME = 'SEL'."V1-2 ADD 设置选择标记的字段名称   选择该行时能够自动刷新
ENDFORM.
*&---------------------------------------------------------------------*
*&----- ALV输出
*&---------------------------------------------------------------------*
FORM ALV_DISPLAY.
  IF C_CYZB = 'X'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = G_REPID
        I_SAVE             = 'X'
        IS_LAYOUT          = LT_LAYO           "输出格式
        IT_FIELDCAT        = LIT_FCAT[]        "字段输出格式
        IT_EVENTS          = GIT_EVENTS[]      "分屏显示
      TABLES
        T_OUTTAB           = GT_CYZB           "输出的内表
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
  ELSE.
    IF C_EXEC = 'X'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          I_CALLBACK_USER_COMMAND  = 'F_USER_COMMAND'
          I_CALLBACK_PF_STATUS_SET = 'F_SETSTATUS'
          I_CALLBACK_PROGRAM       = G_REPID
          I_SAVE                   = 'X'
          IS_LAYOUT                = LT_LAYO           "输出格式
          IT_FIELDCAT              = LIT_FCAT[]        "字段输出格式
          IT_EVENTS                = GIT_EVENTS[]      "分屏显示
        TABLES
          T_OUTTAB                 = GT_KPMX           "输出的内表
        EXCEPTIONS
          PROGRAM_ERROR            = 1
          OTHERS                   = 2.
    ELSE.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          I_CALLBACK_PROGRAM = G_REPID
          I_SAVE             = 'X'
          IS_LAYOUT          = LT_LAYO           "输出格式
          IT_FIELDCAT        = LIT_FCAT[]        "字段输出格式
          IT_EVENTS          = GIT_EVENTS[]      "分屏显示
        TABLES
          T_OUTTAB           = GT_KPMX           "输出的内表
        EXCEPTIONS
          PROGRAM_ERROR      = 1
          OTHERS             = 2.
    ENDIF.
  ENDIF.
ENDFORM.

"*&---------------------------------------------------------------------*
"*& 按钮
"*&---------------------------------------------------------------------*
FORM F_SETSTATUS USING PT_EXTAB TYPE SLIS_T_EXTAB. "固定参数
  SET PF-STATUS 'STANDARD_FULLSCREEN' ."EXCLUDING PT_EXTAB. " 排产按钮
ENDFORM.

"*&---------------------------------------------------------------------*
"*& 事件响应
"*&---------------------------------------------------------------------*
FORM F_USER_COMMAND USING LP_UCOMM TYPE SY-UCOMM   "固定参数
                       LS_SELFIELD TYPE SLIS_SELFIELD.
  CASE LP_UCOMM.
    WHEN '&DATA_SAVE'.
      PERFORM POST_EXEC.
      LS_SELFIELD-REFRESH = 'X'.
  ENDCASE.
ENDFORM.



*&---------------------------------------------------------------------*
*&----- 过账
*&---------------------------------------------------------------------*
FORM POST_EXEC.
  DATA:WAERS TYPE T001-WAERS."货币码
  DATA: L_ITEMNO      TYPE I VALUE 10,
        L_BELNR(10),
        L_MSG         TYPE STRING,
        L_POST_FLAG,
        LT_RETURN_TMP LIKE TABLE OF BAPIRET2.
  CLEAR: LS_HEAD,LT_EXTS,LT_ACGL,LT_CURR,LT_RETURN.
*过账检查
  LOOP AT GT_KPMX INTO GS_KPMX WHERE ERR NE 'X'.
    IF GS_KPMX-BELNR IS NOT INITIAL.
      L_POST_FLAG = 'X'.
    ENDIF.
  ENDLOOP.
  IF L_POST_FLAG = 'X'.
    MESSAGE '该期间已过帐' TYPE 'E'.
  ENDIF.

  SORT GT_KPMX BY AUBEL AUPOS.
  "清空 GT_POST[].
  CLEAR:GS_POST,GT_POST[].
*按成本科目、物料汇总
  LOOP AT GT_KPMX INTO GS_KPMX WHERE ERR NE 'X' AND SHFTF IS NOT INITIAL.
    CLEAR GS_POST.
    GS_POST-AUBEL = GS_KPMX-AUBEL.
    GS_POST-AUPOS = GS_KPMX-AUPOS.
    GS_POST-SAKN2 = GS_KPMX-SAKN2.
    GS_POST-MATNR = GS_KPMX-MATNR.
    GS_POST-SHFTF = GS_KPMX-SHFTF.
    GS_POST-FKIMG = GS_KPMX-FKIMG.
    COLLECT GS_POST INTO GT_POST.
  ENDLOOP.
  IF GT_POST[] IS NOT INITIAL.
*  SELECT  VBELN KUNNR
*    INTO CORRESPONDING FIELDS OF TABLE GT_VBAK
*    FROM VBAK
*    FOR ALL ENTRIES IN GT_POST
*    WHERE VBELN = GT_POST-AUBEL .
    LOOP AT GT_POST INTO GS_POST.
      READ TABLE GT_VBAP WITH KEY VBELN = GS_POST-AUBEL POSNR = GS_POST-AUPOS.
      IF SY-SUBRC = 0.
        GS_POST-WERKS = GT_VBAP-WERKS.
        GS_POST-MEINS = GT_VBAP-MEINS.
      ENDIF.
      READ TABLE GT_VBAK WITH KEY VBELN = GS_POST-AUBEL.
      IF SY-SUBRC = 0 .
        GS_POST-KUNNR = GT_VBAK-KUNNR.


      ENDIF.

      MODIFY GT_POST FROM GS_POST.

    ENDLOOP.



  ENDIF.
  "取公司货币码
  SELECT SINGLE WAERS INTO WAERS FROM T001 WHERE BUKRS = S_BUKRS-LOW .
*
*表头

  LS_HEAD-COMP_CODE =  GS_KPMX-BUKRS.    "公司代码
  LS_HEAD-DOC_DATE  =  L_LASTD.         "凭证日期 -------<< 待修改
  LS_HEAD-PSTNG_DATE = L_LASTD.         "过账日期 改为期间最后一天
  "ls_head-pstng_date = sy-datum.         "过账日期 改为期间最后一天
  LS_HEAD-DOC_TYPE  = 'ML'.               "凭证类型（默认值） 150528 SA 替换为ML
  LS_HEAD-USERNAME  = SY-UNAME.
  LS_HEAD-HEADER_TXT = '产品订单差异还原'.                          "150528 add

  LOOP AT GT_POST INTO GS_POST.
    CLEAR : LT_ACGL ,LT_ACGL[] ,LT_CURR,LT_CURR[],LT_CRITERIA,LT_CRITERIA[],LT_RETURN,LT_RETURN[],LT_EXTS,LT_EXTS[].
    CLEAR: LS_EXTS,LS_ZEXT.
    IF GS_POST-SHFTF < 0.         "总差异小于零，则为借
*记账号-]
      LS_ZEXT-POSNR = L_ITEMNO.   "凭证行项目
      LS_ZEXT-BSCHL = '40'.       "记账码
      LS_EXTS-STRUCTURE = 'ZACCDOCUEXT'.
      LS_EXTS-VALUEPART1 = LS_ZEXT.
      APPEND LS_EXTS TO LT_EXTS.

      LS_ZEXT-POSNR = L_ITEMNO + 10.   "凭证行项目
      LS_ZEXT-BSCHL = '50'.       "记账码
      LS_EXTS-STRUCTURE = 'ZACCDOCUEXT'.
      LS_EXTS-VALUEPART1 = LS_ZEXT.
      APPEND LS_EXTS TO LT_EXTS.

*科目表
      CLEAR LS_ACGL.
      LS_ACGL-ITEMNO_ACC = L_ITEMNO.

*&--代码注释 BY HANDYBY 26.07.2017 14:45:17  BEGIN
*      LS_ACGL-GL_ACCOUNT = '1406010101'.
*&--代码注释 BY HANDYBY 26.07.2017 14:45:17  END
*&--代码添加 BY HANDYBY 26.07.2017 14:39:52  BEGIN
      IF GS_VBRP-BUKRS = '1600' OR
         GS_VBRP-BUKRS = '1610' OR
         GS_VBRP-BUKRS = '1660' OR
         GS_VBRP-BUKRS = '2900' OR
         GS_VBRP-BUKRS = '2910' OR
         GS_VBRP-BUKRS = '1720' OR
         GS_VBRP-BUKRS = '2300' OR
         GS_VBRP-BUKRS = '2310' OR
         GS_VBRP-BUKRS = '2320' .
        LS_ACGL-GL_ACCOUNT = '1406010102'.
      ELSE .
        LS_ACGL-GL_ACCOUNT = '1406010101'.
      ENDIF.
*&--代码添加 BY HANDYBY 26.07.2017 14:39:52  END

      "ls_acgl-customer   = gs_post-kunrg.   "客户
      LS_ACGL-MATERIAL   = GS_POST-MATNR.   "物料
      LS_ACGL-PLANT   = GS_POST-WERKS.   "工厂
      LS_ACGL-ALLOC_NMBR = GS_POST-AUBEL. "销售订单号
      CONCATENATE GS_POST-MATNR '/' '物料分类账差异' INTO LS_ACGL-ITEM_TEXT.
      APPEND LS_ACGL TO LT_ACGL.
*
      CLEAR LS_ACGL.
      LS_ACGL-ITEMNO_ACC = L_ITEMNO + 10.
      LS_ACGL-GL_ACCOUNT = GS_POST-SAKN2.
      "ls_acgl-customer   = gs_post-kunrg.   "客户
      LS_ACGL-MATERIAL   = GS_POST-MATNR.   "物料
      LS_ACGL-PLANT   = GS_POST-WERKS.   "工厂
      LS_ACGL-QUANTITY = ABS( GS_POST-FKIMG )."数量
      LS_ACGL-BASE_UOM = GS_POST-MEINS ."单位
      LS_ACGL-ALLOC_NMBR = GS_POST-AUBEL. "销售订单号
      APPEND LS_ACGL TO LT_ACGL.

*金额+
      CLEAR: LS_CURR.
      LS_CURR-ITEMNO_ACC = L_ITEMNO.   "凭证行项目
      LS_CURR-AMT_DOCCUR = ABS( GS_POST-SHFTF ) ."金额
      LS_CURR-CURRENCY   =  WAERS . "'CNY'.      "货币码
      APPEND LS_CURR TO LT_CURR.

*金额-
      CLEAR: LS_CURR.
      LS_CURR-ITEMNO_ACC = L_ITEMNO + 10.   "凭证行项目
      LS_CURR-AMT_DOCCUR =  GS_POST-SHFTF  ."金额
      LS_CURR-CURRENCY   =  WAERS . "''CNY'.      "货币码
      APPEND LS_CURR TO LT_CURR.

*分配物料号
      CLEAR: LS_CRITERIA.
      LS_CRITERIA-ITEMNO_ACC = L_ITEMNO + 10.  "在成本科目项分配
      LS_CRITERIA-FIELDNAME = 'KAUFN'.  " 销售订单
      LS_CRITERIA-CHARACTER = GS_POST-AUBEL.
      APPEND LS_CRITERIA TO LT_CRITERIA.
      LS_CRITERIA-FIELDNAME = 'KDPOS'.  " 销售订单项目
      LS_CRITERIA-CHARACTER = GS_POST-AUPOS.
      APPEND LS_CRITERIA TO LT_CRITERIA.
      LS_CRITERIA-FIELDNAME = 'KNDNR'.  " 客户
      LS_CRITERIA-CHARACTER = GS_POST-KUNNR.
      APPEND LS_CRITERIA TO LT_CRITERIA.
      LS_CRITERIA-FIELDNAME = 'ARTNR'.  " 生产
      LS_CRITERIA-CHARACTER = GS_POST-MATNR.
      APPEND LS_CRITERIA TO LT_CRITERIA.



    ELSE.
*记账号+
      LS_ZEXT-POSNR = L_ITEMNO.   "凭证行项目
      LS_ZEXT-BSCHL = '40'.       "记账码
      LS_EXTS-STRUCTURE = 'ZACCDOCUEXT'.
      LS_EXTS-VALUEPART1 = LS_ZEXT.
      APPEND LS_EXTS TO LT_EXTS.

      LS_ZEXT-POSNR = L_ITEMNO + 10.   "凭证行项目
      LS_ZEXT-BSCHL = '50'.       "记账码
      LS_EXTS-STRUCTURE = 'ZACCDOCUEXT'.
      LS_EXTS-VALUEPART1 = LS_ZEXT.
      APPEND LS_EXTS TO LT_EXTS.

*科目表
      CLEAR LS_ACGL.
      LS_ACGL-ITEMNO_ACC = L_ITEMNO.
      LS_ACGL-GL_ACCOUNT = GS_POST-SAKN2.
      "ls_acgl-customer   = gs_post-kunrg.   "客户
      LS_ACGL-MATERIAL   = GS_POST-MATNR.   "物料
      LS_ACGL-PLANT   = GS_POST-WERKS.   "工厂
      LS_ACGL-QUANTITY = ABS( GS_POST-FKIMG ) ."数量
      LS_ACGL-BASE_UOM = GS_POST-MEINS ."单位
      LS_ACGL-ALLOC_NMBR = GS_POST-AUBEL. "销售订单号
      APPEND LS_ACGL TO LT_ACGL.
*
      CLEAR LS_ACGL.
      LS_ACGL-ITEMNO_ACC = L_ITEMNO + 10.

*&--代码注释 BY HANDYBY 26.07.2017 14:45:17  BEGIN
*      LS_ACGL-GL_ACCOUNT = '1406010101'.
*&--代码注释 BY HANDYBY 26.07.2017 14:45:17  END
*&--代码添加 BY HANDYBY 26.07.2017 14:39:52  BEGIN
      IF GS_VBRP-BUKRS = '1600' OR
         GS_VBRP-BUKRS = '1610' OR
         GS_VBRP-BUKRS = '1660' OR
         GS_VBRP-BUKRS = '2900' OR
         GS_VBRP-BUKRS = '2910' OR
         GS_VBRP-BUKRS = '1720' OR
         GS_VBRP-BUKRS = '2300' OR
         GS_VBRP-BUKRS = '2310' OR
         GS_VBRP-BUKRS = '2320' .
        LS_ACGL-GL_ACCOUNT = '1406010102'.
      ELSE .
        LS_ACGL-GL_ACCOUNT = '1406010101'.
      ENDIF.
*&--代码添加 BY HANDYBY 26.07.2017 14:39:52  END

      "ls_acgl-customer   = gs_post-kunrg.   "客户
      LS_ACGL-MATERIAL   = GS_POST-MATNR.   "物料
      LS_ACGL-PLANT   = GS_POST-WERKS.   "工厂
      LS_ACGL-ALLOC_NMBR = GS_POST-AUBEL. "销售订单号
      CONCATENATE GS_POST-MATNR '/' '物料分类账差异' INTO LS_ACGL-ITEM_TEXT.
      APPEND LS_ACGL TO LT_ACGL.


*金额-
      CLEAR: LS_CURR.
      LS_CURR-ITEMNO_ACC = L_ITEMNO .   "凭证行项目
      LS_CURR-AMT_DOCCUR =  GS_POST-SHFTF ."金额
      LS_CURR-CURRENCY   =  WAERS . "''CNY'.      "货币码
      APPEND LS_CURR TO LT_CURR.

*金额+
      CLEAR: LS_CURR.
      LS_CURR-ITEMNO_ACC = L_ITEMNO + 10.   "凭证行项目
      LS_CURR-AMT_DOCCUR = 0 - GS_POST-SHFTF   ."金额
      LS_CURR-CURRENCY   =  WAERS . "' 'CNY'.      "货币码
      APPEND LS_CURR TO LT_CURR.

*分配物料号
      CLEAR: LS_CRITERIA.
      LS_CRITERIA-ITEMNO_ACC = L_ITEMNO .  "在成本科目项分配
      LS_CRITERIA-FIELDNAME = 'KAUFN'.  " 销售订单
      LS_CRITERIA-CHARACTER = GS_POST-AUBEL.
      APPEND LS_CRITERIA TO LT_CRITERIA.
      LS_CRITERIA-FIELDNAME = 'KDPOS'.  " 销售订单项目
      LS_CRITERIA-CHARACTER = GS_POST-AUPOS.
      APPEND LS_CRITERIA TO LT_CRITERIA.
      LS_CRITERIA-FIELDNAME = 'KNDNR'.  " 客户
      LS_CRITERIA-CHARACTER = GS_POST-KUNNR.
      APPEND LS_CRITERIA TO LT_CRITERIA.
      LS_CRITERIA-FIELDNAME = 'ARTNR'.  " 生产
      LS_CRITERIA-CHARACTER = GS_POST-MATNR.
      APPEND LS_CRITERIA TO LT_CRITERIA.



    ENDIF.

    L_ITEMNO = L_ITEMNO + 20.
    "*--调用BAPI----------------------------------
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        DOCUMENTHEADER = LS_HEAD
      TABLES
        ACCOUNTGL      = LT_ACGL
        CURRENCYAMOUNT = LT_CURR
        CRITERIA       = LT_CRITERIA
        RETURN         = LT_RETURN
        EXTENSION2     = LT_EXTS.
    READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'S'.
    IF SY-SUBRC = 0.
      L_BELNR = LS_RETURN-MESSAGE_V2+0(10).
      LOOP AT GT_KPMX INTO GS_KPMX WHERE ERR NE 'X' AND AUBEL = GS_POST-AUBEL AND AUPOS = GS_POST-AUPOS AND MATNR = GS_POST-MATNR .
        GS_KPMX-BELNR = L_BELNR.
        MODIFY GT_KPMX FROM GS_KPMX.
      ENDLOOP.
      MODIFY ZCOKPMX FROM TABLE GT_KPMX.
      MODIFY ZCOKPMX_TMP FROM TABLE GT_KPMX.  "临时表
      COMMIT WORK AND WAIT.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
      CONCATENATE '会计凭证' LS_RETURN-MESSAGE_V2+0(10) '过账成功!'
         INTO L_MSG.
      MESSAGE L_MSG TYPE 'S'.
*更新期间记录表
      UPDATE ZCODLVD SET POST = 'X' WHERE BUKRS = S_BUKRS-LOW
                                    AND   GJAHR = S_GJAHR-LOW
                                    AND   MONAT = S_MONAT-LOW.
    ELSE.
      "MESSAGE '过账失败，请检查该数据是否正确' TYPE 'E'.
      "READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
      LOOP AT LT_RETURN INTO LS_RETURN WHERE TYPE = 'E'.
        APPEND LS_RETURN TO LT_RETURN_TMP.
      ENDLOOP.
      READ TABLE LT_RETURN_TMP INTO LS_RETURN INDEX 2.
      IF SY-SUBRC = 0.
        MESSAGE LS_RETURN-MESSAGE TYPE 'E'.
      ELSE.
        MESSAGE '过账失败，请检查数据是否正确' TYPE 'E'.
      ENDIF.
    ENDIF.
*刷新ALV
  ENDLOOP.



ENDFORM.
