class ZCL_IM_ME_CHECK_ALL_ITEMS definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_CHECK_ALL_ITEMS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ME_CHECK_ALL_ITEMS IMPLEMENTATION.


  METHOD IF_EX_ME_CHECK_ALL_ITEMS~RECALCULATE_ITEMS.

    DATA: LT_CMNT         TYPE CATSXT_LONGTEXT_ITAB.
    DATA: LS_CMNT         LIKE LINE OF LT_CMNT.
    DATA: LT_RETURN       TYPE TABLE OF BAPIRET2.
    DATA: LS_RETURN       TYPE BAPIRET2.
    DATA: LT_POTEXTHEADER TYPE TABLE OF BAPIMEPOTEXTHEADER.
    DATA: LS_POTEXTHEADER TYPE BAPIMEPOTEXTHEADER.

    DATA LT_POITEM TYPE TABLE OF  BAPIMEPOITEM.
    DATA LS_POITEM TYPE BAPIMEPOITEM.

    DATA LT_POITEMX TYPE TABLE OF  BAPIMEPOITEMX.
    DATA LS_POITEMX TYPE BAPIMEPOITEMX.

    "BREAK HANDABAP.

    IF SY-UCOMM EQ 'MEREJECT'
      AND IM_X_EKKO-PROCSTAT EQ 8.
      DO." make the rejection text mandatory
        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            IM_TITLE = 'ENTER COMMENT'
*           IM_DISPLAY_MODE       = ' '
*           IM_START_COLUMN       = 10
*           IM_START_ROW          = 10
          CHANGING
            CH_TEXT  = LT_CMNT.
        IF LT_CMNT IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDDO.

      LS_POTEXTHEADER-PO_NUMBER = IM_X_EKKO-EBELN.
      LS_POTEXTHEADER-TEXT_ID   = 'F00'.

      LOOP AT  LT_CMNT INTO LS_CMNT.
        CONCATENATE LS_POTEXTHEADER-TEXT_LINE  LS_CMNT INTO  LS_POTEXTHEADER-TEXT_LINE.
        CLEAR LS_CMNT.
      ENDLOOP.

      APPEND LS_POTEXTHEADER TO LT_POTEXTHEADER.
      CLEAR LS_POTEXTHEADER.

      SUBMIT ZTESTYY AND RETURN.
*      CALL FUNCTION 'BAPI_PO_CHANGE'
*        EXPORTING
*          PURCHASEORDER = IM_X_EKKO-EBELN
**         POHEADER      =
**         POHEADERX     =
**         POADDRVENDOR  =
**         TESTRUN       =
**         MEMORY_UNCOMPLETE            =
**         MEMORY_COMPLETE              =
**         POEXPIMPHEADER               =
**         POEXPIMPHEADERX              =
**         VERSIONS      =
**         NO_MESSAGING  =
**         NO_MESSAGE_REQ               =
**         NO_AUTHORITY  =
**         NO_PRICE_FROM_PO             =
**         PARK_UNCOMPLETE              =
**         PARK_COMPLETE =
**   IMPORTING
**         EXPHEADER     =
**         EXPPOEXPIMPHEADER            =
*        TABLES
*          RETURN        = LT_RETURN
**         POITEM        = LT_POITEM
**         POITEMX       = LT_POITEMX
**         POADDRDELIVERY               =
**         POSCHEDULE    =
**         POSCHEDULEX   =
**         POACCOUNT     =
**         POACCOUNTPROFITSEGMENT       =
**         POACCOUNTX    =
**         POCONDHEADER  =
**         POCONDHEADERX =
**         POCOND        =
**         POCONDX       =
**         POLIMITS      =
**         POCONTRACTLIMITS             =
**         POSERVICES    =
**         POSRVACCESSVALUES            =
**         POSERVICESTEXT               =
**         EXTENSIONIN   =
**         EXTENSIONOUT  =
**         POEXPIMPITEM  =
**         POEXPIMPITEMX =
*          POTEXTHEADER  = LT_POTEXTHEADER
**         POTEXTITEM    =
**         ALLVERSIONS   =
**         POPARTNER     =
**         POCOMPONENTS  =
**         POCOMPONENTSX =
**         POSHIPPING    =
**         POSHIPPINGX   =
**         POSHIPPINGEXP =
**         POHISTORY     =
**         POHISTORY_TOTALS             =
**         POCONFIRMATION               =
**         SERIALNUMBER  =
**         SERIALNUMBERX =
**         INVPLANHEADER =
**         INVPLANHEADERX               =
**         INVPLANITEM   =
**         INVPLANITEMX  =
**         POHISTORY_MA  =
**         NFMETALLITMS  =
*        .
*      READ TABLE LT_RETURN INTO LS_RETURN WITH KEY TYPE = 'E'.
*      IF SY-SUBRC = 0.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            WAIT = 'X'.
*
**        WAIT UP TO 1 SECONDS.
**        MESSAGE 'OK' TYPE 'S'.
**        EXIT.
*      ENDIF.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
