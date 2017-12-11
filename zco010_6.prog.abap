REPORT ZCO010_6.


*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20160324
*& Request       :
*& Descriptions  : 销售副总对应成本中心维护
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&
*&---------------------------------------------------------------------*
"


TABLES:TVKBZ,TVKO,CSKT,ZCO010_6.

TYPES:BEGIN OF WA_DATA,
      BUKRS TYPE BUKRS,
      VKORG TYPE VKORG,
      VKBUR TYPE VKBUR,
      XSFZMS TYPE STRING, "销售副总描述
      KOSTL TYPE KOSTL,
      KOSTLMS TYPE STRING, "成本中心描述
      YXQC  TYPE SY-DATUM,
      UNAME TYPE SY-UNAME, "用户名
      UDATE TYPE D,       "日期
      ZBOX  TYPE C,
     END OF WA_DATA.

DATA:IS_ITEM TYPE WA_DATA.
DATA:IT_ITEM TYPE TABLE OF  WA_DATA.
FIELD-SYMBOLS <IS_ITEM> LIKE LINE OF IT_ITEM.

DATA:IT_ZCO010_6 LIKE TABLE OF ZCO010_6 .
DATA:IT_TVKBT LIKE TABLE OF TVKBT,
     IS_TVKBT TYPE TVKBT.
DATA:IT_CSKT  LIKE TABLE OF CSKT,
     IS_CSKT  TYPE CSKT.
DATA:IT_TVKBZ LIKE TABLE OF TVKBZ,
     IS_TVKBZ TYPE TVKBZ.
DATA:IT_TVKO LIKE TABLE OF TVKO,
     IS_TVKO TYPE TVKO.
DATA:IT_CSKS TYPE TABLE OF CSKS,
     IS_CSKS TYPE CSKS.
DATA:W_BUKRS TYPE BUKRS.

DATA:L_SUBRC LIKE SY-SUBRC .
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:P_BUKRS TYPE BUKRS OBLIGATORY .
SELECTION-SCREEN END OF BLOCK B1.

 "*&---------------------------------------------------------------------*
*& 初始化处理
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& 选择屏幕控制
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*  PERFORM XXXXXXX.
*&---------------------------------------------------------------------*
*& 参数输入检查
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM XXXXXXX.

*&---------------------------------------------------------------------*
*& 程序开始处理
*&---------------------------------------------------------------------*


*权限检查检查公司代码
  PERFORM FRM_AUTH_CHECK USING '03'.
  IF SY-SUBRC NE 0.
    MESSAGE I011(ZFICO01) WITH P_BUKRS DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  PERFORM FRM_GET_DATA. "取数逻辑
  PERFORM FRM_DEAL_DATA. "处理数逻辑
  PERFORM FRM_CALL_SCREEN_SUB. "调用9001屏幕
*&---------------------------------------------------------------------*
*&      Form  FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0113   text
*----------------------------------------------------------------------*
FORM FRM_AUTH_CHECK USING VALUE(P_ACTVT).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'ACTVT' FIELD P_ACTVT
                                      ID 'BUKRS' FIELD P_BUKRS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.

  DATA:STITLE TYPE STRING .
    SET PF-STATUS 'STATE_9001'.
    CONCATENATE P_BUKRS '公司的销售副总对应成本中心维护' INTO STITLE.
    SET TITLEBAR STITLE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
DATA:S_MSG TYPE STRING.
IF SY-UCOMM = '&CANCLE'.
   LEAVE TO SCREEN 0 .
  ELSEIF SY-UCOMM = '&SAVE'.
    CHECK  L_SUBRC =  0 .
    "更新数据表ZCO010_6
    REFRESH IT_ZCO010_6.

    "先根据公司代码删除ZCO010_6表数据
    DELETE  FROM ZCO010_6 WHERE BUKRS = P_BUKRS .
    IF IT_ITEM IS NOT INITIAL.
      MOVE-CORRESPONDING IT_ITEM TO IT_ZCO010_6.
    ENDIF.
    SORT IT_ZCO010_6 BY BUKRS VKORG VKBUR  KOSTL YXQC .
    DELETE ADJACENT  DUPLICATES FROM IT_ZCO010_6 COMPARING BUKRS VKORG VKBUR  KOSTL YXQC .
    MODIFY ZCO010_6 FROM TABLE IT_ZCO010_6.
    IF SY-SUBRC EQ 0 .
       COMMIT WORK.
       MESSAGE '更新数据库表ZCO010_6成功' TYPE 'S'.
     ELSE.
        ROLLBACK WORK.
        MESSAGE '更新数据库表ZCO010_6失败' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0.
     ENDIF.
 ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_GET_DATA .
  "查询销售副总维护的成本中心信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ITEM
  FROM ZCO010_6
  WHERE BUKRS EQ P_BUKRS.
SORT IT_ITEM BY BUKRS VKORG .

"查询主管副总信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TVKBT
  FROM TVKBT
  FOR ALL ENTRIES IN IT_ITEM
  WHERE VKBUR = IT_ITEM-VKBUR.

SORT IT_TVKBT BY VKBUR.

"查询成本中心信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_CSKT
  FROM CSKT
  WHERE
  SPRAS = '1'
  AND KOKRS = '1000' .
SORT IT_CSKT BY KOSTL.

"查询销售办公室信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TVKBZ
  FROM TVKBZ
  WHERE VTWEG = '00' AND SPART = '00'.
SORT IT_TVKBZ BY VKORG VKBUR.

"查询销售机构信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TVKO
  FROM TVKO
  WHERE BUKRS = P_BUKRS.
SORT IT_TVKO BY VKORG.

"查询成本中心信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_CSKS
  FROM CSKS
  WHERE KOKRS = '1000'
  AND  BUKRS = P_BUKRS.
 SORT IT_CSKS BY KOSTL.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CALL_SCREEN_SUB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_CALL_SCREEN_SUB .
 CALL SCREEN 9001 .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_DEAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEAL_DATA .
IF IT_ITEM IS NOT INITIAL.
  LOOP AT IT_ITEM ASSIGNING <IS_ITEM>.

    "查询成本中心主数据信息
  READ TABLE IT_CSKT INTO IS_CSKT
      WITH KEY KOSTL = <IS_ITEM>-KOSTL BINARY SEARCH .
    IF SY-SUBRC EQ 0 .
       <IS_ITEM>-KOSTLMS = IS_CSKT-KTEXT.
    ENDIF.

    "查询销售副总信息
  READ TABLE IT_TVKBT INTO IS_TVKBT
     WITH KEY VKBUR = <IS_ITEM>-VKBUR BINARY SEARCH.
    IF SY-SUBRC EQ 0 .
     <IS_ITEM>-XSFZMS = IS_TVKBT-BEZEI.
   ENDIF.

ENDLOOP.

ELSE.
    APPEND  INITIAL LINE TO IT_ITEM.
ENDIF.

ENDFORM.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TAB_KOSTL' ITSELF
CONTROLS: TAB_KOSTL TYPE TABLEVIEW USING SCREEN 9001.

*&SPWIZARD: LINES OF TABLECONTROL 'TAB_KOSTL'
DATA:     G_TAB_KOSTL_LINES  LIKE SY-LOOPC.

DATA:     OK_CODE LIKE SY-UCOMM.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_KOSTL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TAB_KOSTL_CHANGE_TC_ATTR OUTPUT.
  W_BUKRS = P_BUKRS.
  DESCRIBE TABLE IT_ITEM LINES TAB_KOSTL-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_KOSTL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TAB_KOSTL_GET_LINES OUTPUT.
  G_TAB_KOSTL_LINES = SY-LOOPC.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_KOSTL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE TAB_KOSTL_MODIFY INPUT.

  IS_ITEM-BUKRS = P_BUKRS.   "公司代码
  IS_ITEM-UNAME = SY-UNAME.  "用户名
  IS_ITEM-UDATE = SY-DATUM.    "日期

  MODIFY IT_ITEM
    FROM IS_ITEM
    INDEX TAB_KOSTL-CURRENT_LINE.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_KOSTL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TAB_KOSTL_MARK INPUT.
  DATA: g_TAB_KOSTL_wa2 like line of IT_ITEM.
    if TAB_KOSTL-line_sel_mode = 1
    and IS_ITEM-ZBOX = 'X'.
     loop at IT_ITEM into g_TAB_KOSTL_wa2
       where ZBOX = 'X'.
       g_TAB_KOSTL_wa2-ZBOX = ''.
       modify IT_ITEM
         from g_TAB_KOSTL_wa2
         transporting ZBOX.
     endloop.
  endif.
  MODIFY IT_ITEM
    FROM IS_ITEM
    INDEX TAB_KOSTL-CURRENT_LINE
    TRANSPORTING ZBOX.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TAB_KOSTL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TAB_KOSTL_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TAB_KOSTL'
                              'IT_ITEM'
                              'ZBOX'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                          P_TABLE_NAME
                          P_MARK_NAME
                 CHANGING P_OK      LIKE SY-UCOMM.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA: L_OK              TYPE SY-UCOMM,
         L_OFFSET          TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
   SEARCH P_OK FOR P_TC_NAME.
   IF SY-SUBRC <> 0.
     EXIT.
   ENDIF.
   L_OFFSET = STRLEN( P_TC_NAME ) + 1.
   L_OK = P_OK+L_OFFSET.
*&SPWIZARD: execute general and TC specific operations                 *
   CASE L_OK.
     WHEN 'INSR'.                      "insert row
       PERFORM FCODE_INSERT_ROW USING    P_TC_NAME
                                         P_TABLE_NAME.
       CLEAR P_OK.

     WHEN 'DELE'.                      "delete row
       PERFORM FCODE_DELETE_ROW USING    P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME.
       CLEAR P_OK.

     WHEN 'P--' OR                     "top of list
          'P-'  OR                     "previous page
          'P+'  OR                     "next page
          'P++'.                       "bottom of list
       PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                             L_OK.
       CLEAR P_OK.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
     WHEN 'MARK'.                      "mark all filled lines
       PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME   .
       CLEAR P_OK.

     WHEN 'DMRK'.                      "demark all filled lines
       PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                           P_TABLE_NAME
                                           P_MARK_NAME .
       CLEAR P_OK.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

   ENDCASE.

 ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_insert_row
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_LINES_NAME       LIKE FELD-NAME.
   DATA L_SELLINE          LIKE SY-STEPL.
   DATA L_LASTLINE         TYPE I.
   DATA L_LINE             TYPE I.
   DATA L_TABLE_NAME       LIKE FELD-NAME.
   FIELD-SYMBOLS <TC>                 TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <TABLE>              TYPE STANDARD TABLE.
   FIELD-SYMBOLS <LINES>              TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_LINES_NAME.
   ASSIGN (L_LINES_NAME) TO <LINES>.

*&SPWIZARD: get current line                                           *
   GET CURSOR LINE L_SELLINE.
   IF SY-SUBRC <> 0.                   " append line to table
     L_SELLINE = <TC>-LINES + 1.
*&SPWIZARD: set top line                                               *
     IF L_SELLINE > <LINES>.
       <TC>-TOP_LINE = L_SELLINE - <LINES> + 1 .
     ELSE.
       <TC>-TOP_LINE = 1.
     ENDIF.
   ELSE.                               " insert line into table
     L_SELLINE = <TC>-TOP_LINE + L_SELLINE - 1.
     L_LASTLINE = <TC>-TOP_LINE + <LINES> - 1.
   ENDIF.
*&SPWIZARD: set new cursor line                                        *
   L_LINE = L_SELLINE - <TC>-TOP_LINE + 1.

*&SPWIZARD: insert initial line                                        *
   INSERT INITIAL LINE INTO <TABLE> INDEX L_SELLINE.
   <TC>-LINES = <TC>-LINES + 1.
*&SPWIZARD: set cursor                                                 *
   SET CURSOR LINE L_LINE.

 ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_delete_row
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME
                        P_MARK_NAME   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TABLE_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE cxtab_control.
   FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <WA>.
   FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
   DESCRIBE TABLE <TABLE> LINES <TC>-LINES.

   LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     IF <MARK_FIELD> = 'X'.
       DELETE <TABLE> INDEX SYST-TABIX.
       IF SY-SUBRC = 0.
         <TC>-LINES = <TC>-LINES - 1.
       ENDIF.
     ENDIF.
   ENDLOOP.

 ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME
                                       P_OK.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TC_NEW_TOP_LINE     TYPE I.
   DATA L_TC_NAME             LIKE FELD-NAME.
   DATA L_TC_LINES_NAME       LIKE FELD-NAME.
   DATA L_TC_FIELD_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE cxtab_control.
   FIELD-SYMBOLS <LINES>      TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.
*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
   ASSIGN (L_TC_LINES_NAME) TO <LINES>.


*&SPWIZARD: is no line filled?                                         *
   IF <TC>-LINES = 0.
*&SPWIZARD: yes, ...                                                   *
     L_TC_NEW_TOP_LINE = 1.
   ELSE.
*&SPWIZARD: no, ...                                                    *
     CALL FUNCTION 'SCROLLING_IN_TABLE'
          EXPORTING
               ENTRY_ACT             = <TC>-TOP_LINE
               ENTRY_FROM            = 1
               ENTRY_TO              = <TC>-LINES
               LAST_PAGE_FULL        = 'X'
               LOOPS                 = <LINES>
               OK_CODE               = P_OK
               OVERLAPPING           = 'X'
          IMPORTING
               ENTRY_NEW             = L_TC_NEW_TOP_LINE
          EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
               OTHERS                = 0.
   ENDIF.

*&SPWIZARD: get actual tc and column                                   *
   GET CURSOR FIELD L_TC_FIELD_NAME
              AREA  L_TC_NAME.

   IF SYST-SUBRC = 0.
     IF L_TC_NAME = P_TC_NAME.
*&SPWIZARD: et actual column                                           *
       SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
     ENDIF.
   ENDIF.

*&SPWIZARD: set the new top line                                       *
   <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.


 ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_MARK_LINES USING P_TC_NAME
                               P_TABLE_NAME
                               P_MARK_NAME.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                 P_TABLE_NAME
                                 P_MARK_NAME .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = SPACE.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  SET_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_VALUE INPUT.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'VKBUR'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'IS_ITEM-VKBUR'
      value_org   = 'S'
    TABLES
      value_tab   = IT_TVKBT .
  SELECT SINGLE BEZEI INTO IS_ITEM-XSFZMS FROM TVKBT WHERE VKBUR = IS_ITEM-VKBUR.
  MODIFY IT_ITEM FROM IS_ITEM INDEX TAB_KOSTL-CURRENT_LINE TRANSPORTING XSFZMS.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_SEL_VKBUR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_SEL_VKBUR INPUT.
 DATA:E_MSG1 TYPE STRING .
 CLEAR:L_SUBRC.
 SELECT SINGLE BEZEI INTO IS_ITEM-XSFZMS FROM TVKBT WHERE VKBUR = IS_ITEM-VKBUR.
 READ TABLE IT_TVKBZ INTO IS_TVKBZ WITH KEY VKORG = IS_ITEM-VKORG VKBUR = IS_ITEM-VKBUR BINARY SEARCH .
  IF SY-SUBRC NE 0 .
     L_SUBRC = 4 .
    CONCATENATE '销售组织' IS_ITEM-VKORG '销售副总' IS_ITEM-VKBUR '检查不通过' INTO E_MSG1.
    MESSAGE E_MSG1 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_SEL_KOSTL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_SEL_KOSTL INPUT.
 DATA:E_MSG3 TYPE STRING.
 CLEAR:L_SUBRC.
 SELECT SINGLE LTEXT INTO IS_ITEM-KOSTLMS FROM CSKT WHERE KOSTL = IS_ITEM-KOSTL.
 READ TABLE IT_CSKS INTO IS_CSKS WITH KEY KOSTL = IS_ITEM-KOSTL BINARY SEARCH .
  IF SY-SUBRC NE 0 .
     L_SUBRC = 4 .
    CONCATENATE '公司代码' P_BUKRS '不存在成本中心' IS_ITEM-KOSTL '请核对!' INTO E_MSG3.
    MESSAGE E_MSG3 TYPE 'S'  DISPLAY LIKE 'E'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VKBUR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_VKBUR INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VKORG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_VKORG INPUT.
 DATA:E_MSG2 TYPE STRING.
 CLEAR:L_SUBRC.
 READ TABLE IT_TVKO INTO IS_TVKO WITH KEY VKORG = IS_ITEM-VKORG  BINARY SEARCH .
  IF SY-SUBRC NE 0 .
    L_SUBRC = 4 .
    CONCATENATE '公司代码' P_BUKRS '销售组织' IS_ITEM-VKORG '检查不通过' INTO E_MSG2.
    MESSAGE E_MSG2 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDMODULE.
