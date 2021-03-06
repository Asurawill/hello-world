*&---------------------------------------------------------------------*
*& Report  ZMM0081
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMM0081 MESSAGE-ID ZMM01.

*----------------------------------------------------------------------*
* TYPES OR TYPE-POOLS                                                  *
*----------------------------------------------------------------------*
*屏幕9000，ALV变量定义
TABLES:VBAK,EKKO,ZMM002I.
DATA: G_ERROR,
      SAVE_OK         LIKE SY-UCOMM,
      OK_CODE         LIKE SY-UCOMM,
      GT_FIELDCAT_1   TYPE LVC_T_FCAT WITH HEADER LINE,                                "ALV字段目录
      GT_FIELDCAT_2   TYPE LVC_T_FCAT WITH HEADER LINE,                                "ALV字段目录
      GT_SORT         TYPE LVC_T_SORT,                                       "ALV排序
      GW_VARIANT_1    TYPE DISVARIANT,                                 "布局变式
      GW_EXCLUDE_1    TYPE UI_FUNCTIONS,                                "ALV排除按钮
      GW_LAYOUT_1     TYPE LVC_S_LAYO,                                 "ALV样式
      GW_LAYOUT_2     TYPE LVC_S_LAYO,                                 "ALV样式
      GW_VARIANT_2    TYPE DISVARIANT,                                 "布局变式
      GW_EXCLUDE_2    TYPE UI_FUNCTIONS,                                "ALV排除按钮

      G_DOCK_1        TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      G_DOCK_2        TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      G_ALV_1         TYPE REF TO CL_GUI_ALV_GRID,                      "ALV类
      G_ALV_2         TYPE REF TO CL_GUI_ALV_GRID,                       "ALV类
      G_BEHAVIOUR_ALV TYPE REF TO CL_DRAGDROP.


DATA REPID TYPE SY-REPID.
DATA DYNNR TYPE SY-DYNNR.
DATA G_INIT9000.
DATA G_DBDH_BEGIN TYPE ZDBDH VALUE 1000000000. "起始调拨单号
*&SPWIZARD: DECLARATION OF TABLECONTROL 'TAB_STOCK' ITSELF
CONTROLS: TAB_STOCK TYPE TABLEVIEW USING SCREEN 9001.
*&SPWIZARD: LINES OF TABLECONTROL 'TAB_STOCK'
DATA:     G_TAB_STOCK_LINES  LIKE SY-LOOPC.
DATA BEGIN OF IT_DATA1 OCCURS 0.
        INCLUDE TYPE ZMM002I.
DATA:
  MAKTX       TYPE MAKTX,
  SGTXT       TYPE MSEG-SGTXT,
  EDITFLAG    TYPE C,
  KDMAT       TYPE VBAP-KDMAT,
  KDBNUM      TYPE KWMENG,
  BEDAE       TYPE VBAP-BEDAE, "按销售订单调拨 特殊库存标示 如果为KEV，则为E；如果为KSV，则为空；
  "UMSOK      TYPE EKPO-UMSOK, "按采购订单调拨 特殊库存标示 直接取ekpo-umsok； 直接 as spcid into spcid
  FIELD_STYLE TYPE LVC_T_STYL,
  ZBOX        TYPE C,
  LOCKED      TYPE ICON_D,
  END OF IT_DATA1.

DATA : IT_DATA_S  LIKE TABLE OF IT_DATA1 WITH HEADER LINE.
DATA : IT_DATA2   LIKE TABLE OF ZMM002I WITH HEADER LINE.
DATA : IT_DATA3   LIKE TABLE OF IT_DATA1 WITH HEADER LINE.
DATA : IT_DATA_S2 LIKE TABLE OF ZMM002I WITH HEADER LINE.
DATA : IT_ZMM002I TYPE ZMM002I OCCURS 0 WITH HEADER LINE.
DATA:  IT_EKPO LIKE TABLE OF EKPO WITH HEADER LINE.
DATA:BEGIN OF IT_SEL_STOCK OCCURS 0,
       MATNR  TYPE MATNR,
       WERKS  TYPE ZMM002I-WERKS,
       LGORT  TYPE ZMM002I-SLOCFR,
       LGOBE  TYPE LGOBE,
       CHARG  TYPE CHARG_D,
       VBELN  TYPE VBELN_VA,
       POSNR  TYPE POSNR_VA,
       KALAB  TYPE LABST,
       LGORTO TYPE ZMM002I-STOCTO,
       LABST  TYPE LABST,
     END OF IT_SEL_STOCK.
data: gv_lgort LIKE t001l-lgort.
data: gv_CHARG TYPE  CHARG_D.
DATA: GV_CANCEL.

DATA WA_SEL_STOCK LIKE LINE OF IT_SEL_STOCK.
DATA G_DBD_NUM TYPE LABST ."调拨单已创建对应订单调拨总数量
DATA G_DBD_MARGIN TYPE LABST ."调拨单可创建余量临时记录字段

DATA:L_EBELN TYPE EKKN-EBELN.
DATA:LW_VBAK TYPE VBAK.
DATA:L_MESSAGE TYPE STRING.
************************************************************************
*                    SELECTION-SCREEN                                  *
************************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.
PARAMETER:
  P_VBELN TYPE VBAK-VBELN MATCHCODE OBJECT VMVA.
SELECT-OPTIONS:  S_DBDH  FOR ZMM002I-DBDH.
* s_auart FOR vbak-auart ,
* s_audat FOR vbak-audat ,
* s_vkorg FOR vbak-vkorg
.
PARAMETER :RAD1 RADIOBUTTON GROUP RADA DEFAULT 'X',
           RAD2 RADIOBUTTON GROUP RADA.

SELECTION-SCREEN END OF SCREEN 100.

*定义子屏幕200 SUBSCREEN
SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
PARAMETER:
          P_EBELN TYPE EKKO-EBELN MATCHCODE OBJECT MEKK_C.
SELECT-OPTIONS:  S_DBDH1  FOR ZMM002I-DBDH.
PARAMETER :RAD3 RADIOBUTTON GROUP RADB DEFAULT 'X',
           RAD4 RADIOBUTTON GROUP RADB.
SELECTION-SCREEN END OF SCREEN 200.

*选择页TAB功能,定义两个TAB
SELECTION-SCREEN:
 BEGIN OF TABBED BLOCK MYTAB FOR 10 LINES,
  TAB (20) BUTTON1 USER-COMMAND PUSH1,  "（20）代表要显示在TAB内容的长度，最大长度为79，不相信?你可以试下.哈哈
  TAB (20) BUTTON2 USER-COMMAND PUSH2,
 END OF BLOCK MYTAB.


INCLUDE ZMM0081CLASS. " CLASS

INITIALIZATION.
  BUTTON1 = '参照销售订单进行调拨（非公司间业务）'.
  BUTTON2 = '参照公司间采购订单进行调拨（公司间业务）'.
  MYTAB-PROG = SY-REPID. "指定调用的程序名
  MYTAB-DYNNR = 100."指定当前要显示的屏幕
  MYTAB-ACTIVETAB = 'PUSH1'.

*PERFORM frm_init.

AT SELECTION-SCREEN.
  CASE SY-UCOMM.
    WHEN 'PUSH1'.
      MYTAB-DYNNR = 100.
      MYTAB-ACTIVETAB = 'PUSH1'.
    WHEN 'PUSH2'.
      MYTAB-DYNNR = 200.
      MYTAB-ACTIVETAB = 'PUSH2'.
  ENDCASE.


*  PERFORM frm_auth_check.
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_VBELN' OR SCREEN-NAME = 'P_EBELN'.
      SCREEN-REQUIRED = '2'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*  PERFORM frm_authority_check.
START-OF-SELECTION.
  CASE MYTAB-ACTIVETAB.
    WHEN  'PUSH1'.

      IF P_VBELN = ''.
        MESSAGE S003 WITH 'VBELN' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.

      SELECT SINGLE VBELN VKORG INTO CORRESPONDING FIELDS OF LW_VBAK
        FROM VBAK WHERE VBELN = P_VBELN.
      IF SY-SUBRC = 0 AND LW_VBAK-VKORG = '1000'.
        L_MESSAGE = '该订单为北京销售订单，请通过公司间采购业务调拨，对应采购订单为：'.
        SELECT SINGLE  EBELN  FROM EKKN INTO L_EBELN
          WHERE VBELN = P_VBELN.
        IF SY-SUBRC = 0.
          CONCATENATE L_MESSAGE L_EBELN INTO L_MESSAGE.
        ELSE.
          CONCATENATE L_MESSAGE '空' INTO L_MESSAGE.
        ENDIF.
        MESSAGE L_MESSAGE  TYPE 'I' ."DISPLAY LIKE 'A'.
        LEAVE LIST-PROCESSING.


      ENDIF.

    WHEN  'PUSH2'.
      IF P_EBELN = ''.
        MESSAGE S003 WITH 'EBELN' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

END-OF-SELECTION.
  PERFORM FRM_GET_DATA.

  IF S_DBDH IS NOT INITIAL.
    SELECT * FROM ZMM002I
     INTO CORRESPONDING FIELDS OF TABLE IT_DATA2
     WHERE DBDH IN S_DBDH
     AND   ZDELFLAG <> 'X'.
*     AND   REVERSAL <> 'X'.
  ELSEIF S_DBDH1 IS NOT INITIAL.
    SELECT * FROM ZMM002I
    INTO CORRESPONDING FIELDS OF TABLE IT_DATA2
    WHERE DBDH IN S_DBDH1
    AND   ZDELFLAG <> 'X'.
*    AND   REVERSAL <> 'X'.
  ENDIF.

  APPEND INITIAL LINE TO IT_DATA2.

  CALL SCREEN 9000.

  INCLUDE ZMM0081O01. " PBO-Modules
  INCLUDE ZMM0081I01. " PAI-Modules
  INCLUDE ZMM0081F01. " FORM-Routines
