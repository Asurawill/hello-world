class ZCL_IM_MB_MIGO_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_MIGO_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MB_MIGO_BADI IMPLEMENTATION.


  method IF_EX_MB_MIGO_BADI~CHECK_HEADER.
  endmethod.


  METHOD IF_EX_MB_MIGO_BADI~CHECK_ITEM.
*&--代码添加 BY HANDYBY 22.05.2017 14:29:45  BEGIN

    DATA CS_GOITEM TYPE GOITEM .
    IMPORT CS_GOITEM = CS_GOITEM FROM MEMORY ID 'CS_GOITEM'.

    DATA L_LEN TYPE I VALUE IS INITIAL .
    L_LEN = STRLEN( CS_GOITEM-MATNR ).
    L_LEN = L_LEN - 12 .
    IF CS_GOITEM-WERKS = '160Y' AND CS_GOITEM-BWART = '101' AND CS_GOITEM-KNTTP = 'A'.
      IF CS_GOITEM-MATNR+L_LEN(12) <> CS_GOITEM-ANLN1 .
        MESSAGE '物料号后12位与固定资产编号不一致！' TYPE 'E' DISPLAY LIKE 'I'.
      ENDIF.
*      IF CS_GOITEM-EKORG = '1600'.
*        IF CS_GOITEM-ANLN2 EQ '0000' .
*          MESSAGE '固定资产次级编号必输！' TYPE 'E' DISPLAY LIKE 'I'.
*        ENDIF.
*      ENDIF.
    ENDIF.
*
*&--代码添加 BY HANDYBY 22.05.2017 14:29:45  END

  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_DELETE.
*    BREAK HANDYBY .
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_LOAD.
*    BREAK HANDYBY .
  endmethod.


  method IF_EX_MB_MIGO_BADI~HOLD_DATA_SAVE.
*    BREAK HANDYBY .
  endmethod.


  method IF_EX_MB_MIGO_BADI~INIT.
  endmethod.


  method IF_EX_MB_MIGO_BADI~LINE_DELETE.
  endmethod.


  METHOD IF_EX_MB_MIGO_BADI~LINE_MODIFY.

*科目类别为空，正常采购需要质检
*    IF CS_GOITEM-KNTTP IS INITIAL AND ( CS_GOITEM-BWART = '101' OR CS_GOITEM-BWART = '102').
*      MESSAGE E022(ZMM01) WITH CS_GOITEM-EBELN CS_GOITEM-EBELP CS_GOITEM-MATNR.
*    ENDIF.
*
*科目类别不为空，非质检采购
*    IF CS_GOITEM-KNTTP IS NOT INITIAL AND ( CS_GOITEM-BWART = '103' OR CS_GOITEM-BWART = '104'
*      OR CS_GOITEM-BWART = '105' OR CS_GOITEM-BWART = '106').
*      MESSAGE E023(ZMM01) WITH CS_GOITEM-EBELN CS_GOITEM-EBELP CS_GOITEM-MATNR.
*    ENDIF.

    IF ( CS_GOITEM-WERKS = '1100' OR CS_GOITEM-WERKS = '1000' OR CS_GOITEM-WERKS = '1500' OR CS_GOITEM-WERKS = '2110'

*&--代码添加 BY HANDYBY 17.05.2017 15:55:08  BEGIN
      or CS_GOITEM-WERKS = '1610'
*&--代码添加 BY HANDYBY 17.05.2017 15:55:08  END

      ) AND CS_GOITEM-EBELN IS NOT INITIAL .

*科目类别为空,强制 移动类型101.科目类别非用，强制 移动类型 103.
      IF ( CS_GOITEM-KNTTP = 'K' OR CS_GOITEM-KNTTP = 'A' OR CS_GOITEM-KNTTP = 'F' ) AND CS_GOITEM-BWART = '103'.
        CS_GOITEM-BWART = '101'.
      ELSEIF ( CS_GOITEM-KNTTP <> 'K' AND CS_GOITEM-KNTTP <>'A' AND CS_GOITEM-KNTTP <> 'F') AND  CS_GOITEM-BWART = '101'.
        CS_GOITEM-BWART = '103'.
      ENDIF.

*当移动类型为105，106时候，默认为非限制库存
      IF CS_GOITEM-BWART = '105' OR CS_GOITEM-BWART = '106' OR CS_GOITEM-BWART = '161' OR CS_GOITEM-BWART = '162'.
        CS_GOITEM-MIGO_INSMK = ''.
      ENDIF.
    ENDIF.

*&--代码添加 BY HANDYBY 22.05.2017 14:29:21  BEGIN
  EXPORT CS_GOITEM = CS_GOITEM TO MEMORY ID 'CS_GOITEM' .
*&--代码添加 BY HANDYBY 22.05.2017 14:29:21  END

  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~MODE_SET.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PAI_DETAIL.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PAI_HEADER.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PBO_DETAIL.
  endmethod.


  method IF_EX_MB_MIGO_BADI~PBO_HEADER.

  endmethod.


  METHOD IF_EX_MB_MIGO_BADI~POST_DOCUMENT.

    DATA:IT_DATA  TYPE TABLE OF MSEG,
         IT_DATA1 TYPE TABLE OF MSEG,
         WA_DATA  TYPE MSEG,
         WA_DATA1 TYPE MSEG,
         WA_MSEG  TYPE MSEG.

    DATA:MENGE  TYPE MSEG-MENGE.
    DATA:MENGE1 TYPE MSEG-MENGE.
    DATA:MENGE2 TYPE MSEG-MENGE.
    DATA:L_STRING TYPE STRING.

    CLEAR:MENGE,MENGE1,MENGE2.

    IF IT_MSEG IS NOT INITIAL.

      SELECT MBLNR
             MJAHR
             ZEILE
             AUFNR
             MENGE
             BWART
             SHKZG
             XAUTO
             LGORT
             MATNR
             XAUTO
             WERKS
        FROM MSEG
        INTO CORRESPONDING FIELDS OF TABLE IT_DATA
        FOR ALL ENTRIES IN IT_MSEG
        WHERE AUFNR = IT_MSEG-AUFNR
        AND WERKS = IT_MSEG-WERKS
        AND LGORT = IT_MSEG-LGORT
        AND MATNR = IT_MSEG-MATNR
        AND ( BWART = 'Z13' OR BWART = 'Z14' )
        AND XAUTO = 'X'.

    ENDIF.

    LOOP AT IT_MSEG INTO WA_MSEG WHERE XAUTO = 'X' AND BWART = 'Z14'.

      LOOP AT IT_DATA INTO WA_DATA
      WHERE AUFNR = WA_MSEG-AUFNR
        AND WERKS = WA_MSEG-WERKS
        AND LGORT = WA_MSEG-LGORT
        AND MATNR = WA_MSEG-MATNR
        AND ( BWART = 'Z13' OR BWART = 'Z14' )
        AND XAUTO = 'X'.

        IF WA_DATA-SHKZG = 'H'.
          WA_DATA-MENGE = - WA_DATA-MENGE.
        ENDIF.

        IF WA_DATA-BWART = 'Z13'.
          MENGE = MENGE + WA_DATA-MENGE.
        ENDIF.

        IF WA_DATA-BWART = 'Z14'.
          MENGE1 = MENGE1 + WA_DATA-MENGE.
        ENDIF.

        CLEAR WA_DATA.
      ENDLOOP.

      MENGE2 = MENGE + MENGE1.

      IF WA_MSEG-MENGE > MENGE2.
        CLEAR L_STRING.
        CONCATENATE '物料' WA_MSEG-MATNR '整盘领料退回数量大于领料数量，请重新输入' INTO L_STRING.
        MESSAGE L_STRING TYPE 'E' DISPLAY LIKE 'I'.
        EXIT.
      ELSE.
        CLEAR:MENGE,
              MENGE1,
              MENGE2.
        CONTINUE.
      ENDIF.




    ENDLOOP.

*&--代码添加 BY HANDYBY 21.06.2017 11:34:24  BEGIN
* 项目自动发料
*    DATA CS_GOITEM TYPE GOITEM .
*    IMPORT CS_GOITEM = CS_GOITEM FROM MEMORY ID 'CS_GOITEM'.
*    DATA: LT_ZPSKC TYPE TABLE OF ZPSKC,
*          LS_ZPSKC TYPE ZPSKC.
*    DATA L_FLAG TYPE C .
*    IF CS_GOITEM-BWART = '101' AND CS_GOITEM-SOBKZ = 'Q'.
*      SELECT SINGLE *
*        INTO LS_ZPSKC
*        FROM ZPSKC
*       WHERE WERKS = CS_GOITEM-WERKS .
*      IF SY-SUBRC = 0  .
*        CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
*          EXPORTING
**           DEFAULTOPTION = 'Y'
*            DIAGNOSETEXT1 = '是否发货至项目'
**           DIAGNOSETEXT2 = ' '
**           DIAGNOSETEXT3 = ' '
*            TEXTLINE1     = ''
**           TEXTLINE2     = ' '
*            TITEL         = '发货判断'
**           START_COLUMN  = 25
**           START_ROW     = 6
**           CANCEL_DISPLAY       = 'X'
*          IMPORTING
*            ANSWER        = L_FLAG.
*      ELSE.
*      ENDIF.
*    ENDIF.
*&--代码添加 BY HANDYBY 21.06.2017 11:34:24  END


*    ENDIF.
  ENDMETHOD.


  method IF_EX_MB_MIGO_BADI~PROPOSE_SERIALNUMBERS.

  endmethod.


  method IF_EX_MB_MIGO_BADI~PUBLISH_MATERIAL_ITEM.
*    BREAK HANDYBY .
  endmethod.


  method IF_EX_MB_MIGO_BADI~RESET.
*    BREAK HANDYBY .
  endmethod.


  method IF_EX_MB_MIGO_BADI~STATUS_AND_HEADER.
*    BREAK HANDYBY .
  endmethod.
ENDCLASS.
