class ZCL_IM_MB_DOCUMENT_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MB_DOCUMENT_BADI IMPLEMENTATION.


  METHOD IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE.

* cread by handwy 2015-1-27
* 在工厂1200、1201、1210、1211下，物料主数据的价格控制MBEW-VPRSV为”S”时，
*则不允许MB1A、MB1B、MB1C、MIGO、MB31、MBST、MB01、MB11、ME2O、VL01N、VL02N、VL09、VF01、VF02、VF11等事务代码的操作
* 运行上述事物码时报错“当前无发布的标准成本，请联系财务进行标准成本发布”
*   IF sy-uname = 'HANDLQ' OR sy-uname = 'SKLFICO' .

*    DATA:LW_MSEG TYPE MSEG.
*    DATA:LT_MBEW TYPE STANDARD TABLE OF MBEW,
*         LW_MBEW TYPE                   MBEW.
*    DATA:L_MSG TYPE STRING.
*
*    IF XMSEG IS NOT INITIAL.
*      SELECT MATNR
*        BWKEY " 评估范围
*        VPRSV " 价格控制指示符
*        STPRS
*        INTO CORRESPONDING FIELDS OF TABLE LT_MBEW
*        FROM MBEW
*        FOR ALL ENTRIES IN  XMSEG
*        WHERE MATNR = XMSEG-MATNR
*        AND BWKEY = XMSEG-WERKS.
*    ENDIF.
*
*    LOOP AT XMSEG INTO LW_MSEG.
**      IF LW_MSEG-SOBKZ <> 'B'. " 特殊库存标识
**        IF LW_MSEG-WERKS = '1200' OR LW_MSEG-WERKS = '1201'
**          OR LW_MSEG-WERKS = '1210' OR LW_MSEG-WERKS = '1211'.
*      READ TABLE LT_MBEW INTO LW_MBEW WITH KEY MATNR = LW_MSEG-MATNR
*                                  BWKEY = LW_MSEG-WERKS.
*      IF SY-SUBRC = 0.
*        IF  LW_MBEW-VPRSV = 'S' AND LW_MBEW-STPRS IS INITIAL . " 没有发布标准成本
*          IF SY-TCODE = 'MIGO'.
*            CLEAR:L_MSG.
*            CONCATENATE '物料号' LW_MSEG-MATNR INTO L_MSG.
*            CALL FUNCTION 'POPUP_TO_INFORM'
*              EXPORTING
*                TITEL = '错误消息'
*                TXT1  = L_MSG
*                TXT2  = '该物料无标准成本，请联系财务部发布物料的标准成本'.
*            LEAVE PROGRAM.
*          ELSE.
*            CLEAR:L_MSG.
*            CONCATENATE '物料号' LW_MSEG-MATNR '当前无发布的标准成本，请联系财务进行标准成本发布' INTO L_MSG.
*            MESSAGE  L_MSG  TYPE  'E'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
**        ENDIF.
**      ENDIF.
*    ENDLOOP.
*    ENDIF.

*&--代码添加 BY HANDYBY 22.06.2017 12:05:28  BEGIN

    EXPORT XMKPF = XMKPF
               XMSEG = XMSEG TO MEMORY ID 'XMIGO' .
*
*&--代码添加 BY HANDYBY 22.06.2017 12:05:28  END

*    BREAK  HANDYBY .
*    DATA LS_MKPF LIKE LINE OF XMKPF .
*    DATA LS_MSEG LIKE LINE OF XMSEG .
*    DATA: LT_ZPSKC TYPE TABLE OF ZPSKC,
*          LS_ZPSKC TYPE ZPSKC.
*    DATA L_INDEX TYPE C .
*    DATA L_FLAG TYPE C .
*    DATA L_DIAGNOSETEXT1 TYPE STRING .
*
*    DATA LS_HEADER TYPE BAPI2017_GM_HEAD_01 .
*    DATA LS_CODE TYPE BAPI2017_GM_CODE .
*    DATA LS_HEADRET TYPE BAPI2017_GM_HEAD_RET .
*    DATA LS_DOCUMENT TYPE BAPI2017_GM_HEAD_RET-MAT_DOC .
*    DATA LS_YEAR TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR .
*    DATA: LT_ITEM TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
*          LS_ITEM TYPE BAPI2017_GM_ITEM_CREATE.
*    DATA: LT_RETURN TYPE TABLE OF BAPIRET2,
*          LS_RETURN TYPE BAPIRET2.
*
*    SELECT *
*           INTO TABLE LT_ZPSKC
*           FROM ZPSKC
*       FOR ALL ENTRIES IN XMSEG
*          WHERE WERKS = XMSEG-WERKS .
*
*    READ TABLE XMKPF INTO LS_MKPF INDEX 1 .
*    IF LS_MKPF-MBLNR IS NOT INITIAL .
*
**      COMMIT WORK .
*
*      LOOP AT XMSEG INTO LS_MSEG .
*        IF LS_MSEG-BWART = '101' AND LS_MSEG-SOBKZ = 'Q'.
*          READ TABLE LT_ZPSKC INTO LS_ZPSKC WITH KEY WERKS = LS_MSEG-WERKS .
*          IF SY-SUBRC = 0 .
*            L_INDEX = 'X'.
*
*            LS_ITEM-MATERIAL = LS_MSEG-MATNR .
*            LS_ITEM-PLANT = LS_MSEG-WERKS .
*            LS_ITEM-STGE_LOC = LS_MSEG-LGORT .
*            LS_ITEM-MOVE_TYPE = 'Z19'.
*            LS_ITEM-SPEC_STOCK = 'Q'.
*            LS_ITEM-ENTRY_QNT = LS_MSEG-MENGE .
*            LS_ITEM-ENTRY_UOM = LS_MSEG-MEINS .
*            CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*              EXPORTING
*                INPUT  = LS_MSEG-PS_PSP_PNR
*              IMPORTING
*                OUTPUT = LS_ITEM-VAL_WBS_ELEM.
**            LS_ITEM-VAL_WBS_ELEM = LS_MSEG-PS_PSP_PNR .
*            APPEND LS_ITEM TO LT_ITEM  .
*            CLEAR LS_ITEM .
*            CLEAR LS_ZPSKC .
*          ENDIF.
*        ENDIF.
*        CLEAR LS_MSEG .
*      ENDLOOP.
*
*      IF  L_INDEX = 'X'.
*
*        CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
*          EXPORTING
**           DEFAULTOPTION = 'Y'
*            DIAGNOSETEXT1 = '是否发货至项目？'
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
*
*        IF L_FLAG = 'J'.
*
*          LS_HEADER-PSTNG_DATE = LS_MKPF-BUDAT .
*          LS_HEADER-DOC_DATE = LS_MKPF-BLDAT .
*
*          LS_CODE-GM_CODE = '03'.
*
*          CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*            EXPORTING
*              GOODSMVT_HEADER  = LS_HEADER
*              GOODSMVT_CODE    = LS_CODE
**             TESTRUN          = ' '
**             GOODSMVT_REF_EWM =
*            IMPORTING
*              GOODSMVT_HEADRET = LS_HEADRET
*              MATERIALDOCUMENT = LS_DOCUMENT
*              MATDOCUMENTYEAR  = LS_YEAR
*            TABLES
*              GOODSMVT_ITEM    = LT_ITEM
**             GOODSMVT_SERIALNUMBER         =
*              RETURN           = LT_RETURN
**             GOODSMVT_SERV_PART_DATA       =
**             EXTENSIONIN      =
*            .
*
*          READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'E'.
*          IF SY-SUBRC = 0 .
**            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
**            MESSAGE LS_RETURN-MESSAGE TYPE 'I' DISPLAY LIKE 'E' .
*          ELSE.
*            CLEAR LS_RETURN .
*            READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'A'.
*            IF SY-SUBRC = 0 .
**              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
**              MESSAGE LS_RETURN-MESSAGE TYPE 'I' DISPLAY LIKE 'E' .
*            ELSE.
**              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**                EXPORTING
**                  WAIT = 'X'.
**              MESSAGE '项目收发货成功！' TYPE 'I' DISPLAY LIKE 'S' .
*            ENDIF.
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*    CLEAR L_INDEX .
*    CLEAR L_FLAG .
*    CLEAR:LS_HEADER,LS_CODE,LS_HEADRET,LS_DOCUMENT,LS_YEAR.
*    CLEAR:LT_ITEM,LT_RETURN.
*    REFRESH:LT_ITEM[],LT_RETURN[].
*    CLEAR LS_MKPF .

  ENDMETHOD.


  METHOD IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_UPDATE.

*&--代码添加 BY HANDYBY 22.05.2017 14:29:21  BEGIN
*    EXPORT XMKPF = XMKPF
*           XMSEG = XMSEG TO MEMORY ID 'XMIGO' .

*    DATA CS_GOITEM TYPE GOITEM .
*    IMPORT CS_GOITEM = CS_GOITEM FROM MEMORY ID 'CS_GOITEM'.
*
*    DATA L_LEN TYPE I VALUE IS INITIAL .
*    L_LEN = STRLEN( CS_GOITEM-MATNR ).
*    L_LEN = L_LEN - 12 .
*    IF CS_GOITEM-WERKS = '160Y' AND CS_GOITEM-BWART = '101' AND CS_GOITEM-KNTTP = 'A'.
*      IF CS_GOITEM-MATNR+L_LEN(12) <> CS_GOITEM-ANLN1 .
*        MESSAGE '物料号后12位与固定资产编号不一致！' TYPE 'E' DISPLAY LIKE 'I'.
*      ENDIF.
**      IF CS_GOITEM-EKORG = '1600'.
**        IF CS_GOITEM-ANLN2 EQ '0000' .
**          MESSAGE '固定资产次级编号必输！' TYPE 'E' DISPLAY LIKE 'I'.
**        ENDIF.
**      ENDIF.
*    ENDIF.
*
*
***    BREAK  HANDYBY .
*    DATA LS_MKPF LIKE LINE OF XMKPF .
*    DATA LS_MSEG LIKE LINE OF XMSEG .
*    DATA: LT_ZPSKC TYPE TABLE OF ZPSKC,
*          LS_ZPSKC TYPE ZPSKC.
*    DATA L_INDEX TYPE C .
*    DATA L_FLAG TYPE C .
*    DATA L_DIAGNOSETEXT1 TYPE STRING .
*
*    DATA LS_HEADER TYPE BAPI2017_GM_HEAD_01 .
*    DATA LS_CODE TYPE BAPI2017_GM_CODE .
*    DATA LS_HEADRET TYPE BAPI2017_GM_HEAD_RET .
*    DATA LS_DOCUMENT TYPE BAPI2017_GM_HEAD_RET-MAT_DOC .
*    DATA LS_YEAR TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR .
*    DATA: LT_ITEM TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
*          LS_ITEM TYPE BAPI2017_GM_ITEM_CREATE.
*    DATA: LT_RETURN TYPE TABLE OF BAPIRET2,
*          LS_RETURN TYPE BAPIRET2.
*
*    SELECT *
*           INTO TABLE LT_ZPSKC
*           FROM ZPSKC
*       FOR ALL ENTRIES IN XMSEG
*          WHERE WERKS = XMSEG-WERKS .
*
*    READ TABLE XMKPF INTO LS_MKPF INDEX 1 .
*    IF LS_MKPF-MBLNR IS NOT INITIAL .
*
**      COMMIT WORK .
*
*      LOOP AT XMSEG INTO LS_MSEG .
*        IF LS_MSEG-BWART = '101' AND LS_MSEG-SOBKZ = 'Q'.
*          READ TABLE LT_ZPSKC INTO LS_ZPSKC WITH KEY WERKS = LS_MSEG-WERKS .
*          IF SY-SUBRC = 0 .
*            L_INDEX = 'X'.
*
*            LS_ITEM-MATERIAL = LS_MSEG-MATNR .
*            LS_ITEM-PLANT = LS_MSEG-WERKS .
*            LS_ITEM-STGE_LOC = LS_MSEG-LGORT .
*            LS_ITEM-MOVE_TYPE = 'Z19'.
*            LS_ITEM-SPEC_STOCK = 'Q'.
*            LS_ITEM-ENTRY_QNT = LS_MSEG-MENGE .
*            LS_ITEM-ENTRY_UOM = LS_MSEG-MEINS .
*            CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*              EXPORTING
*                INPUT  = LS_MSEG-PS_PSP_PNR
*              IMPORTING
*                OUTPUT = LS_ITEM-VAL_WBS_ELEM.
**            LS_ITEM-VAL_WBS_ELEM = LS_MSEG-PS_PSP_PNR .
*            APPEND LS_ITEM TO LT_ITEM  .
*            CLEAR LS_ITEM .
*            CLEAR LS_ZPSKC .
*          ENDIF.
*        ENDIF.
*        CLEAR LS_MSEG .
*      ENDLOOP.
*
*      IF  L_INDEX = 'X'.
*
*        CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
*          EXPORTING
**           DEFAULTOPTION = 'Y'
*            DIAGNOSETEXT1 = '是否发货至项目？'
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
*
*        IF L_FLAG = 'J'.
*
*          LS_HEADER-PSTNG_DATE = LS_MKPF-BUDAT .
*          LS_HEADER-DOC_DATE = LS_MKPF-BLDAT .
*
*          LS_CODE-GM_CODE = '03'.
*
*          CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*            EXPORTING
*              GOODSMVT_HEADER  = LS_HEADER
*              GOODSMVT_CODE    = LS_CODE
**             TESTRUN          = ' '
**             GOODSMVT_REF_EWM =
*            IMPORTING
*              GOODSMVT_HEADRET = LS_HEADRET
*              MATERIALDOCUMENT = LS_DOCUMENT
*              MATDOCUMENTYEAR  = LS_YEAR
*            TABLES
*              GOODSMVT_ITEM    = LT_ITEM
**             GOODSMVT_SERIALNUMBER         =
*              RETURN           = LT_RETURN
**             GOODSMVT_SERV_PART_DATA       =
**             EXTENSIONIN      =
*            .
*
*          READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'E'.
*          IF SY-SUBRC = 0 .
**            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
**            MESSAGE LS_RETURN-MESSAGE TYPE 'I' DISPLAY LIKE 'E' .
*          ELSE.
*            CLEAR LS_RETURN .
*            READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'A'.
*            IF SY-SUBRC = 0 .
**              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
**              MESSAGE LS_RETURN-MESSAGE TYPE 'I' DISPLAY LIKE 'E' .
*            ELSE.
**              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**                EXPORTING
**                  WAIT = 'X'.
**              MESSAGE '项目收发货成功！' TYPE 'I' DISPLAY LIKE 'S' .
*            ENDIF.
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*    CLEAR L_INDEX .
*    CLEAR L_FLAG .
*    CLEAR:LS_HEADER,LS_CODE,LS_HEADRET,LS_DOCUMENT,LS_YEAR.
*    CLEAR:LT_ITEM,LT_RETURN.
*    REFRESH:LT_ITEM[],LT_RETURN[].
*    CLEAR LS_MKPF .
  ENDMETHOD.
ENDCLASS.
