*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"              SAP_ACTVT LIKE  AUTHA-PS_ACTVT
*"              SAP_PROJ LIKE  PROJ STRUCTURE  PROJ
*"              SAP_MSGTY LIKE  SY-MSGTY DEFAULT SPACE
*"       EXPORTING
*"              SAP_X_ACTVT LIKE  RCJ_MARKL-MARK
*"---------------------------------------------

IF SY-TCODE = 'ZCJ20N'.
  CLEAR LS_PROJ.
  LS_PROJ = SAP_PROJ.

  IF FLAG <> 'X'.
    CALL SCREEN 9000
     STARTING AT 1  1
     ENDING   AT 50 10.
  ENDIF.

  IF FLAG1 = 'X'.
    SAP_X_ACTVT = 'X'.
  ENDIF.

ELSE.
  SAP_X_ACTVT = 'X'.
ENDIF.
*  IF SY-UNAME <> 'HANDWY'.
*    SAP_X_ACTVT = 'X'.
*  ENDIF.

*&--代码添加 BY HANDYBY 18.07.2017 19:04:14  BEGIN
*BREAK-POINT .
DATA L_MSG TYPE STRING .

DATA L_BUKRS TYPE T001-BUKRS.
IF SAP_PROJ-VBUKR IS NOT INITIAL .
  L_BUKRS = SAP_PROJ-VBUKR .
ELSE .
  L_BUKRS = SAP_PROJ-PSPID+0(4) .
ENDIF.

AUTHORITY-CHECK OBJECT 'ZPS_BUKRS'
ID 'ZPS_BUKRS' FIELD L_BUKRS .
IF SY-SUBRC NE 0 .
  CONCATENATE '你没有权限查看' L_BUKRS '公司代码下的项目' INTO L_MSG .
* Initialize the messages
  CALL FUNCTION 'MESSAGES_INITIALIZE'
    EXCEPTIONS
      LOG_NOT_ACTIVE       = 1
      WRONG_IDENTIFICATION = 2
      OTHERS               = 3.
* Store the messages to be displayed
  DATA: P_MSGID LIKE SMESG-ARBGB,
        P_MSGTY LIKE SMESG-MSGTY.
  DATA: P_MSGV1 TYPE STRING,
        P_MSGV2 TYPE STRING,
        P_MSGV3 TYPE STRING,
        P_MSGV4 TYPE STRING,
        P_TXTNR TYPE STRING.
  P_MSGID = '错误' .
  P_MSGTY = 'E'.
  P_MSGV1 = L_MSG .
*  P_MSGV2
*  P_MSGV3
*  P_MSGV4
  P_TXTNR = '001'.
  CALL FUNCTION 'MESSAGE_STORE'
    EXPORTING
      ARBGB                  = P_MSGID
      MSGTY                  = P_MSGTY
      MSGV1                  = P_MSGV1
      MSGV2                  = P_MSGV2
      MSGV3                  = P_MSGV3
      MSGV4                  = P_MSGV4
      TXTNR                  = P_TXTNR
    EXCEPTIONS
      MESSAGE_TYPE_NOT_VALID = 1
      NOT_ACTIVE             = 2
      OTHERS                 = 3.
  "at last call the below function module to show the messages ata time..
* Display all the messages together on a pop up
  DATA L_EXIT_COMMAND TYPE BAL_S_EXCM.
  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
      SHOW_LINNO         = SPACE
    IMPORTING
      E_EXIT_COMMAND     = L_EXIT_COMMAND
    EXCEPTIONS
      INCONSISTENT_RANGE = 1
      NO_MESSAGES        = 2
      OTHERS             = 3.
  IF L_EXIT_COMMAND-CONTINUE EQ 'X' OR L_EXIT_COMMAND-CANCEL EQ 'X' .
    LEAVE PROGRAM .
  ENDIF.
ENDIF.
*&--代码添加 BY HANDYBY 18.07.2017 19:04:14  END
