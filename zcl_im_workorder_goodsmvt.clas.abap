class ZCL_IM_WORKORDER_GOODSMVT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_WORKORDER_GOODSMVT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_WORKORDER_GOODSMVT IMPLEMENTATION.


  method IF_EX_WORKORDER_GOODSMVT~BACKFLUSH.
  endmethod.


  METHOD IF_EX_WORKORDER_GOODSMVT~COGI_AUTHORITY_CHECK.
  ENDMETHOD.


  method IF_EX_WORKORDER_GOODSMVT~COGI_POST.
  endmethod.


  METHOD IF_EX_WORKORDER_GOODSMVT~COMPLETE_GOODSMOVEMENT.
  ENDMETHOD.


  METHOD IF_EX_WORKORDER_GOODSMVT~GM_SCREEN_LINE_CHECK.
  ENDMETHOD.


  method IF_EX_WORKORDER_GOODSMVT~GM_SCREEN_OKCODE_CHECK.
  endmethod.


  METHOD IF_EX_WORKORDER_GOODSMVT~GM_WIPBATCH_CHECK.
*    IF SY-UNAME = 'HANDWY'.
*      BREAK-POINT.
*    ENDIF.
  ENDMETHOD.


  method IF_EX_WORKORDER_GOODSMVT~GM_WIPBATCH_PROPOSE.
  endmethod.


  METHOD IF_EX_WORKORDER_GOODSMVT~GOODS_RECEIPT.
*ADD BY HANDWY.  2015-2-20 生产订单报工时，对其组件的发料情况进行检查
    IF SY-TCODE = 'CO11N'.

      DATA LS_GOODS_RECEIPT LIKE LINE OF CT_GOODS_RECEIPT.
      DATA LS_AFKO TYPE AFKO.
      DATA LT_RESB TYPE TABLE OF RESB.
      DATA LS_RESB TYPE RESB.
      DATA LS_AFPO TYPE AFPO.

      CLEAR:LS_AFKO,
            LS_RESB.
      REFRESH  LT_RESB.

      LOOP AT CT_GOODS_RECEIPT INTO LS_GOODS_RECEIPT.
        IF LS_GOODS_RECEIPT-BWART = '101'.
*查询需求跟踪号
          SELECT SINGLE * FROM AFKO
            INTO CORRESPONDING FIELDS OF LS_AFKO
            WHERE AUFNR = LS_GOODS_RECEIPT-AUFNR.

*查询已经入库的数量
          SELECT SINGLE * FROM AFPO
            INTO CORRESPONDING FIELDS OF LS_AFPO
            WHERE AUFNR = LS_GOODS_RECEIPT-AUFNR.

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
            IF LS_RESB-ENMNG < ( ( LS_GOODS_RECEIPT-ERFMG + LS_AFPO-WEMNG ) / LS_AFKO-GAMNG * LS_RESB-BDMNG ).
              MESSAGE S005(ZPP01) DISPLAY LIKE 'E'.
              LEAVE TO TRANSACTION  'CO11N'.
            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDLOOP.

    ENDIF.
    "ENDADD.
  ENDMETHOD.


  method IF_EX_WORKORDER_GOODSMVT~IM_CALLED.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~MANUAL_GOODS_RECEIPT.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~PICKLIST.
  endmethod.
ENDCLASS.
