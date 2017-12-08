*&---------------------------------------------------------------------*
*&  包含                ZXMBCU02
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_MSEG) LIKE  MSEG STRUCTURE  MSEG
*"             VALUE(I_VM07M) LIKE  VM07M STRUCTURE  VM07M
*"             VALUE(I_DM07M) LIKE  DM07M STRUCTURE  DM07M
*"             VALUE(I_MKPF) LIKE  MKPF STRUCTURE  MKPF
*"       EXPORTING
*"             VALUE(E_SGTXT) LIKE  MSEG-SGTXT
*"----------------------------------------------------------------------
                                                            "MBCF0002
IF I_MSEG-WERKS = '1500' AND I_MSEG-MATNR <> '' AND I_MSEG-AUFNR <> ''AND ( I_MSEG-BWART = '101' OR I_MSEG-BWART = '102' )  .
  DATA: WA_AFPO TYPE AFPO, WA_RESB TYPE RESB.

  DATA LS_AFKO TYPE AFKO.
  DATA LT_RESB TYPE TABLE OF RESB.
  DATA LS_RESB TYPE RESB.
  DATA LS_AFPO TYPE AFPO.

  CLEAR:LS_AFKO,
        LS_RESB.
  REFRESH  LT_RESB.

*SELECT  *
*  INTO WA_AFPO FROM AFPO WHERE AUFNR = I_MSEG-AUFNR .
*  SELECT * INTO WA_RESB  FROM  RESB WHERE AUFNR = WA_AFPO-AUFNR  AND XLOEK <> 'X' AND KZEAR <> 'X'AND XWAOK = 'X' AND BDMNG <> '0' AND SHKZG = 'H' .
*  IF SY-SUBRC = 0.
*    MESSAGE '生产订单没有完全发料，请先完成发料在做入库操作' TYPE 'E'.
*  ENDIF.
*ENDSELECT.
*ENDSELECT.

*查询需求跟踪号
  SELECT SINGLE * FROM AFKO
    INTO CORRESPONDING FIELDS OF LS_AFKO
    WHERE AUFNR = I_MSEG-AUFNR.

*查询已经入库的数量
  SELECT SINGLE * FROM AFPO
    INTO CORRESPONDING FIELDS OF LS_AFPO
    WHERE AUFNR = I_MSEG-AUFNR.

*查询订单BON
  SELECT * FROM RESB
    INTO CORRESPONDING FIELDS OF TABLE LT_RESB
    WHERE RSNUM = LS_AFKO-RSNUM
    AND   DUMPS <> 'X'  "排除虚拟项目
    AND   XLOEK <> 'X'  "排除已删除项目
    AND   KZEAR <> 'X'  "排除最后交货标识
    AND   RGEKZ <> 'X' ."排除反冲

*收货数量要加上已经入库的数量AFPO-WEMNG
*如果存在行项目物料 提货数量<[（收货数量÷订单数量）×组件需求数量] 则报错
  LOOP AT LT_RESB INTO LS_RESB.
    IF LS_RESB-ENMNG < ( ( I_MSEG-ERFMG + LS_AFPO-WEMNG ) / LS_AFKO-GAMNG * LS_RESB-BDMNG ).
      MESSAGE E005(ZPP01).
    ENDIF.
  ENDLOOP.
ENDIF.

"DESC:生产入库时，检查生产订单的组件投料套数、报工数量、是否允许超入库 by it02 20170711 begin
"检查条件：2110 移动类型：101 订单：不为空 限制收货数量标志:为空  AFPO-UEBTK为空
IF ( I_MSEG-WERKS = '2110'
*&--代码添加 BY HANDYBY 19.07.2017 14:31:53  BEGIN
  OR I_MSEG-WERKS = '2900' OR I_MSEG-WERKS = '2910' )"OR I_MSEG-WERKS = '1610' )
*&--代码添加 BY HANDYBY 19.07.2017 14:31:53  END
    AND I_MSEG-AUFNR <> '' AND  I_MSEG-BWART = '101' .

  "定义预留数据字段类型
  TYPES:BEGIN OF TY_RESB,
          RSNUM TYPE RESB-RSNUM,  "预留号
          RSPOS TYPE RESB-RSPOS,
          RSART TYPE RESB-RSART,
          XLOEK TYPE RESB-XLOEK,  "删除标识
          KZEAR TYPE RESB-KZEAR,  "最后发货
          BDMNG TYPE RESB-BDMNG,  "需求数量
          ENMNG TYPE RESB-ENMNG,  "提货数量
          TS    TYPE RESB-ENMNG,   "套数
        END OF TY_RESB .


  "定义内表
  DATA:GS_AFPO  TYPE AFPO,             "订单项
       GS_AFKO  TYPE AFKO,             "订单表头数据PP订单
       GS_RESB  TYPE TY_RESB,          "预留类型
       GT_RESB  TYPE TABLE OF TY_RESB,
       G_ZJTLTS TYPE RESB-BDMNG,       "组件投料套数
       G_MIN    TYPE AFKO-GAMNG.

  DATA:G_XX1 TYPE STRING,
       G_XX2 TYPE STRING,
       G_XX3 TYPE STRING,
       G_XX  TYPE STRING.


  "先判断收货明细的生产订单数据的限制收货数量标记是否为空 。
  CLEAR:GS_AFPO.
  SELECT SINGLE * INTO GS_AFPO FROM AFPO WHERE AUFNR = I_MSEG-AUFNR AND UEBTK EQ ''.
  IF SY-SUBRC EQ 0 .
    "根据生产订单取出生产订单数据
    CLEAR:GS_AFKO.
    SELECT SINGLE * INTO GS_AFKO FROM AFKO WHERE AUFNR = I_MSEG-AUFNR .
    IF SY-SUBRC EQ 0..

      "取出生产订单项相关预留项目数据排除删除标记为空、最后发货标记为空。
      REFRESH:GT_RESB.
      SELECT RSNUM RSPOS RSART XLOEK KZEAR BDMNG ENMNG INTO TABLE GT_RESB
      FROM RESB
      WHERE RSNUM = GS_AFKO-RSNUM
        AND BDMNG NE 0
        AND XLOEK EQ ''
        AND KZEAR EQ'' .
      SORT GT_RESB BY  RSNUM .

      "计算预留项目套数
      LOOP AT GT_RESB INTO GS_RESB .

        IF GS_RESB-BDMNG NE 0 .

          GS_RESB-TS = GS_RESB-ENMNG / GS_RESB-BDMNG *  GS_AFKO-GAMNG .

        ENDIF.

        MODIFY GT_RESB FROM  GS_RESB .
      ENDLOOP.

      "排序只保留预留相关最小套数的项目
      SORT GT_RESB BY RSNUM ASCENDING TS ASCENDING .
      DELETE ADJACENT DUPLICATES FROM GT_RESB COMPARING RSNUM .

      "读取投料套数计算结果最小值，若最小值大于订单数量，则输出订单数量；否则输出最小值
      CLEAR G_ZJTLTS.
      READ TABLE GT_RESB INTO GS_RESB WITH KEY RSNUM = GS_AFKO-RSNUM BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        CALL FUNCTION 'ROUND'
          EXPORTING
            DECIMALS      = 0       " 保留多少位小数
            INPUT         = GS_RESB-TS
            SIGN          = '+'     " + 向上取舍 - 向下取舍 （负数也一样）
          IMPORTING
            OUTPUT        = GS_RESB-TS     " 输出返回结果
          EXCEPTIONS
            INPUT_INVALID = 1
            OVERFLOW      = 2
            TYPE_INVALID  = 3
            OTHERS        = 4.
        IF GS_RESB-TS > GS_AFKO-GAMNG .
          G_ZJTLTS = GS_AFKO-GAMNG .
        ELSE.
          G_ZJTLTS  = GS_RESB-TS .
        ENDIF.
      ELSE.
        G_ZJTLTS = GS_AFKO-GAMNG .
      ENDIF.

      IF G_ZJTLTS > GS_AFKO-IGMNG.
        G_MIN = GS_AFKO-IGMNG .

      ELSE.
        G_MIN = G_ZJTLTS .
      ENDIF.
    ENDIF.



    " 若满足 本次入库数量（MIGO界面的数量） <=  { MIN(组件投料套数，报工数量）-已入库数量 }，则校验通过判断错误信息
    IF  I_MSEG-ERFMG + GS_AFPO-WEMNG > G_MIN  .
      CLEAR:G_XX1,G_XX2,G_XX3,G_XX.
      "累计入库数量 = 本次入库数量 + 已入库数量
      G_XX1 = I_MSEG-ERFMG + GS_AFPO-WEMNG.
      G_XX2 = G_MIN. "领料套数
      G_XX3 = GS_AFKO-IGMNG. "报工数量
      CONCATENATE '累计入库数量('  G_XX1 ')'  '超过领料套数(' G_XX2 ')' '或者报工数量（'  G_XX3 ')' INTO G_XX.
      MESSAGE G_XX TYPE 'E'.

    ENDIF.
  ENDIF.
ENDIF.


*&--代码添加 BY HANDYBY 18.08.2017 10:28:06  BEGIN
SELECT SINGLE *
FROM ZZT_ENTH_CONFIG
INTO @DATA(LW_ZZT_ENTH_CONFIG)
WHERE ZZENTH_ID = 'FICO03'
  AND ZZENTH_KEY1 = @I_MSEG-WERKS
  AND ZZENTH_KEY2 = @I_MSEG-BWART
  AND ZZENTH_ACTIVE = 'X'.
IF SY-SUBRC = 0
  AND I_MSEG-SMBLN <> ''.
  "取冲销凭证过账日期
  SELECT SINGLE BUDAT
    INTO @DATA(L_BUDAT)
    FROM MKPF
    WHERE MBLNR = @I_MSEG-SMBLN
      AND MJAHR = @I_MSEG-SJAHR.
  IF SY-SUBRC = 0
    AND L_BUDAT+0(6) <> I_MKPF-BUDAT+0(6).
    MESSAGE '取消日期与确认日期不在同一个期间，不能跨月冲销交货！'(001)  TYPE 'E'.
  ENDIF.
ENDIF.
*&--代码添加 BY HANDYBY 18.08.2017 10:28:06  END




"DESC:生产入库时，检查生产订单的组件投料套数、报工数量、是否允许超入库 by it02 20170711 end
E_SGTXT = I_MSEG-SGTXT .
