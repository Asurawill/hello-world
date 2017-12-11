REPORT ZHROM01.
TABLES:PERNR.
TYPE-POOLS:SLIS.
TYPES:
   BEGIN OF GST_DATA ,
   HROBJIDJT TYPE ORGEH,"集团组织
   HROBJIDKG  TYPE ORGEH,"控股
   HROBJIDC1 TYPE ORGEH,"一级组织
   HROBJIDC2 TYPE ORGEH,"二级组织
   HROBJIDC3 TYPE ORGEH,"三级组织
   HROBJIDC4 TYPE ORGEH,"四级组织
   BMRS     TYPE i  ,"部门人数
   ZBFB     TYPE P LENGTH 10 DECIMALS 2 ,
   ZBFB_1   TYPE STRING ,"占百分比
   HROBJIDJTNAME TYPE STRING ,"集团组织名称
   HROBJIDKGNAME TYPE STRING ,"控股组织名称
   C10RGEN  TYPE STRING , "一级部门
   C20RGEN TYPE STRING, "二级部门
   C30RGEN TYPE STRING, "三级部门
   C40RGEN TYPE STRING , "四级部门
   GJRQ    TYPE SY-DATUM,"关键日期
   BOX ,
 "   PERNR  TYPE PERNR ,
     END OF GST_DATA.
 DATA: GS_DATA TYPE GST_DATA.
 TYPES:BEGIN OF GST_ZZCJ,
       ORGEH   TYPE ORGEH, "部门层级
       SHORT   TYPE SHORT_D,
       BMRS     TYPE i  ,
   END OF GST_ZZCJ .
 DATA:GS_ZZCJ   TYPE GST_ZZCJ.
 DATA:GT_ZZCJ   TYPE STANDARD TABLE OF GST_ZZCJ.
 DATA: GT_DATA TYPE STANDARD TABLE OF GST_DATA.
 DATA:GS_OUT  TYPE GST_DATA.
 DATA:GT_OUT TYPE STANDARD TABLE OF GST_DATA.
 DATA:GT_PA0001  TYPE STANDARD TABLE OF PA0001,
      GT_PA0000  TYPE STANDARD TABLE OF PA0000,
      GT_HRP1001 TYPE STANDARD TABLE OF HRP1001,
      GT_HRP1000 TYPE STANDARD TABLE OF HRP1000.

" ***  define field-symbols
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
               <FS_T500P>   TYPE T500P,
               <FS_PA0009>  TYPE PA0009,
               <FS_CSKT>    TYPE CSKT,
               <FS_T001>    TYPE T001.

 RANGES: S_ORGEH_SEL  FOR PA0001-ORGEH.

  "**INTERNAL TABLE DECLARTION
DATA :
  GR_ALV     TYPE REF TO CL_SALV_TABLE,
  GR_COLUMNS TYPE REF TO CL_SALV_COLUMNS_TABLE.

DATA: IT_FIELDCAT TYPE  SLIS_T_FIELDCAT_ALV WITH HEADER LINE,

      G_SAVE      TYPE C VALUE 'X',
      G_VARIANT   TYPE DISVARIANT,
      GX_VARIANT  TYPE DISVARIANT,
      G_EXIT      TYPE C,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GW_EVENTS   TYPE SLIS_ALV_EVENT.
 DATA:
    L_LAYOUT        TYPE  SLIS_LAYOUT_ALV,
    L_GRID_SETTINGS TYPE  LVC_S_GLAY.
 DATA: Z_TOTAL  TYPE  I.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
PARAMETERS P_DATE TYPE SY-DATUM  OBLIGATORY.
SELECT-OPTIONS:
        S_ORGEH FOR PERNR-ORGEH NO-EXTENSION NO INTERVALS OBLIGATORY.      "部门
SELECTION-SCREEN END OF BLOCK B2.
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S02.
PARAMETERS:G_G1 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G2 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G3 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G4 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G5 TYPE CHAR1 RADIOBUTTON GROUP G1,
           G_G6 TYPE CHAR1 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B3.

*** INITIALIZATION
INITIALIZATION.

*** SCREEN PBO
AT SELECTION-SCREEN OUTPUT.
*  PERFORM PBO.

*** search help 添加 选择组织结构的搜索帮助
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ORGEH-LOW. "
  PERFORM HROBJID_HELP.

START-OF-SELECTION .

    "检查组织权限
  PERFORM   CHECK_ORG .

  PERFORM   SEL_PERSON_DATA.

  PERFORM   DEL_ORG_DATA.

  "填充字段目录
  PERFORM INITIAL_FIELDCAT.

  "填充格式
  PERFORM INITIAL_LAYOUT.

  "调用ALV
  PERFORM DISPLAY_ALV.
*&---------------------------------------------------------------------*
*&      Form  HROBJID_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HROBJID_HELP .
 DATA: PHROBJID TYPE STRING OCCURS 0 WITH HEADER LINE.

  REFRESH: S_ORGEH[].

  CALL FUNCTION 'RP_PNP_ORGSTRUCTURE'
    EXPORTING
      BEGDA  = SY-DATUM
      ENDDA  = SY-DATUM
      PLVAR  = '01'
    TABLES
      POBJID = PHROBJID[].

  LOOP AT PHROBJID.

    IF SY-TABIX = 1.
      CLEAR: S_ORGEH.
      S_ORGEH-SIGN   = PHROBJID(1).
      S_ORGEH-OPTION = PHROBJID+1(2).
      S_ORGEH-LOW    = PHROBJID+3(8).
      APPEND S_ORGEH.
   ENDIF.

    CLEAR S_ORGEH_SEL.
    S_ORGEH_SEL-SIGN   = PHROBJID(1).
    S_ORGEH_SEL-OPTION = PHROBJID+1(2).
    S_ORGEH_SEL-LOW    = PHROBJID+3(8).
    APPEND S_ORGEH_SEL.
        CLEAR GS_ZZCJ .
    GS_ZZCJ-ORGEH = S_ORGEH_SEL-LOW .
    "获取组织层级编码
   DATA:IM_OBJID TYPE OBJEKTID .
    IM_OBJID = GS_ZZCJ-ORGEH .
   CALL FUNCTION 'HR_HCP_READ_OBJECT_TEXT'
     EXPORTING
       IM_PLVAR            = '01'
       IM_OTYPE            = 'O'
*      IM_VIEW_HROBJID       =
*      IM_VIEW_KOKRS       =
       IM_OBJID            =  IM_OBJID
*      IM_ISTAT            = ' '
       IM_BEGDA            = P_DATE
       IM_ENDDA            = P_DATE
    IMPORTING
       SHORT               = GS_ZZCJ-SHORT
*      LONG                =
             .
   APPEND GS_ZZCJ TO GT_ZZCJ .
   SORT GT_ZZCJ BY SHORT ORGEH  .

   ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_ORG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_ORG .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_PERSON_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEL_PERSON_DATA .
  "果部门条件不为空
*  IF S_ORGEH[] IS NOT INITIAL AND S_ORGEH_SEL[] IS INITIAL.
*    CLEAR S_ORGEH_SEL.
*    S_ORGEH_SEL-SIGN   = S_ORGEH-SIGN.
*    S_ORGEH_SEL-OPTION = S_ORGEH-OPTION.
*    S_ORGEH_SEL-LOW    = S_ORGEH-LOW.
*    APPEND S_ORGEH_SEL.
*    CLEAR GS_ZZCJ .
*    GS_ZZCJ-ORGEH = S_ORGEH-LOW .
*    "获取组织层级编码
*   DATA:IM_OBJID TYPE OBJEKTID .
*    IM_OBJID = GS_ZZCJ-ORGEH .
*   CALL FUNCTION 'HR_HCP_READ_OBJECT_TEXT'
*     EXPORTING
*       IM_PLVAR            = '01'
*       IM_OTYPE            = 'O'
**      IM_VIEW_HROBJID       =
**      IM_VIEW_KOKRS       =
*       IM_OBJID            =  IM_OBJID
**      IM_ISTAT            = ' '
*       IM_BEGDA            = P_DATE
*       IM_ENDDA            = P_DATE
*    IMPORTING
*       SHORT               = GS_ZZCJ-SHORT
**      LONG                =
*             .
*   APPEND GS_ZZCJ TO GT_ZZCJ .
*   SORT GT_ZZCJ BY SHORT ORGEH  .
*  ENDIF.
"取关键日期  、组织结构中的编号信息
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE GT_PA0001
    FROM PA0001
    WHERE   ORGEH IN S_ORGEH_SEL "组织编号
    AND (  BEGDA <= P_DATE AND ENDDA >= P_DATE ) .

IF GT_PA0001[] IS NOT INITIAL.
 "取关键日期人员状态
    SELECT *
      INTO TABLE GT_PA0000
      FROM PA0000
      FOR ALL ENTRIES IN GT_PA0001
    WHERE PERNR = GT_PA0001-PERNR
     AND (  BEGDA <= P_DATE AND ENDDA >= P_DATE ) .
ENDIF.

"取关键日期组织信息
SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_HRP1000
  FROM HRP1000
  WHERE PLVAR = '01'
  AND  OTYPE = 'O'
  AND (  BEGDA <= P_DATE AND ENDDA >= P_DATE ) .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DEL_ORG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEL_ORG_DATA .
 FIELD-SYMBOLS: <LFS_PA0001> TYPE PA0001.
 FIELD-SYMBOLS: <LFS_ZZCJ>  TYPE GST_ZZCJ.
  DATA:HROBJID TYPE  HRP1000-OBJID.
      DATA:STRU_TAB LIKE TABLE OF   QCAT_STRU WITH HEADER LINE.

             "    <LFS_OUT>    TYPE TY_OUT.
  SORT GT_PA0001   BY ORGEH  PERNR."组织人员信息根据 组织编号 人员 升序

  "统计选择屏幕组织列表的人员总数
   LOOP AT GT_ZZCJ  ASSIGNING <LFS_ZZCJ> .
      CLEAR GS_DATA.
       "逐一统计每个组织编号人员个数 ，需排除离职人员
      LOOP AT GT_PA0001 ASSIGNING <LFS_PA0001> WHERE ORGEH = <LFS_ZZCJ>-ORGEH .
            READ TABLE  GT_PA0000 ASSIGNING <FS_PA0000> WITH KEY  PERNR = <LFS_PA0001>-PERNR  STAT2 = '0' .
              IF SY-SUBRC NE 0.
                  <LFS_ZZCJ>-BMRS =   <LFS_ZZCJ>-BMRS +  1 .
               ENDIF.
      ENDLOOP.
       Z_TOTAL = Z_TOTAL + <LFS_ZZCJ>-BMRS . "Z_TOAL 存储 所有组织编号 的总人数
       "GS_DATA 存储遍历当前查找组织 的 信息包括 集团 、控股公司 、一级部门 、二级部门、三级部门 、四级部门 及人数
     IF <LFS_ZZCJ>-SHORT = 'A0' .
       GS_DATA-HROBJIDJT = <LFS_ZZCJ>-ORGEH .  "
       GS_DATA-BMRS = <LFS_ZZCJ>-BMRS .  " 只存储 集团 、人数
       APPEND GS_DATA TO GT_DATA.
       CONTINUE .
     ENDIF.

     IF <LFS_ZZCJ>-SHORT = 'B0' .
      " 查找上级集团组织编码
      PERFORM SELUP_ORG  USING 'A0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDJT . "
      GS_DATA-HROBJIDKG = <LFS_ZZCJ>-ORGEH .
      GS_DATA-BMRS = <LFS_ZZCJ>-BMRS .        "只存储 集团、控股公司 、人数
       APPEND GS_DATA TO GT_DATA.
       CONTINUE .
     ENDIF.

       IF <LFS_ZZCJ>-SHORT = 'C1' .
      " 查找上级集团组织编码
      PERFORM SELUP_ORG  USING 'A0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDJT .
      "上级控股公司组织编码
      PERFORM SELUP_ORG  USING 'B0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDKG .
      GS_DATA-HROBJIDC1 =  <LFS_ZZCJ>-ORGEH .
      GS_DATA-BMRS = <LFS_ZZCJ>-BMRS .     "只存储 集团、控股公司 、一级部门、人数
       APPEND GS_DATA TO GT_DATA.
       CONTINUE .
     ENDIF.

     IF <LFS_ZZCJ>-SHORT = 'C2' .
      " 查找上级集团组织编码
      PERFORM SELUP_ORG  USING 'A0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDJT .
      "上级控股公司组织编码
      PERFORM SELUP_ORG  USING 'B0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDKG .
      "上找 C1 的组织编码
       PERFORM SELUP_ORG  USING 'C1' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDC1 .
      GS_DATA-HROBJIDC2 =  <LFS_ZZCJ>-ORGEH .   "只存储 集团、控股公司 、一级部门、二级部门 、人数
      GS_DATA-BMRS = <LFS_ZZCJ>-BMRS .
       APPEND GS_DATA TO GT_DATA.
       CONTINUE .
     ENDIF.

     IF <LFS_ZZCJ>-SHORT = 'C3' .
      " 查找上级集团组织编码
      PERFORM SELUP_ORG  USING 'A0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDJT .
      "上级控股公司组织编码
      PERFORM SELUP_ORG  USING 'B0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDKG .
      "上找 C1 的组织编码
       PERFORM SELUP_ORG  USING 'C1' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDC1 .
      "上找 C2 的组织编码
       PERFORM SELUP_ORG  USING 'C2' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDC2 .
      GS_DATA-HROBJIDC3 =  <LFS_ZZCJ>-ORGEH .    "只存储 集团、控股公司 、一级部门、二级部门 、三级部门 、人数
      GS_DATA-BMRS = <LFS_ZZCJ>-BMRS .
       APPEND GS_DATA TO GT_DATA.
       CONTINUE .
     ENDIF.

      IF <LFS_ZZCJ>-SHORT = 'C4' .
      " 查找上级集团组织编码
      PERFORM SELUP_ORG  USING 'A0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDJT .
      "上级控股公司组织编码
      PERFORM SELUP_ORG  USING 'B0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDKG .
      "上找 C1 的组织编码
       PERFORM SELUP_ORG  USING 'C1' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDC1 .
      "上找 C2 的组织编码
       PERFORM SELUP_ORG  USING 'C2' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDC2 .
         "上找 C3 的组织编码
       PERFORM SELUP_ORG  USING 'C3' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDC3 .
      GS_DATA-HROBJIDC4 =  <LFS_ZZCJ>-ORGEH .
      GS_DATA-BMRS = <LFS_ZZCJ>-BMRS .
       APPEND GS_DATA TO GT_DATA.   "只存储 集团、控股公司 、一级部门、二级部门 、三级部门 、四级部门 、人数
       CONTINUE .
     ENDIF.

     IF <LFS_ZZCJ>-SHORT > 'C4'. "若查找组织层级 >C4
      " 查找上级集团组织编码
      PERFORM SELUP_ORG  USING 'A0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDJT .
      "上级控股公司组织编码
      PERFORM SELUP_ORG  USING 'B0' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDKG .
      "上找 C1 的组织编码
       PERFORM SELUP_ORG  USING 'C1' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDC1 .
      "上找 C2 的组织编码
       PERFORM SELUP_ORG  USING 'C2' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDC2 .
         "上找 C3 的组织编码
       PERFORM SELUP_ORG  USING 'C3' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDC3 .
        "上找 C4 的组织编码
       PERFORM SELUP_ORG  USING 'C4' <LFS_ZZCJ>-ORGEH  CHANGING  GS_DATA-HROBJIDC4 .
       GS_DATA-BMRS = <LFS_ZZCJ>-BMRS .
       APPEND GS_DATA TO GT_DATA. "只存储 集团、控股公司 、一级部门、二级部门 、三级部门 、四级部门 、人数
       CONTINUE .
     ENDIF.
   ENDLOOP.
   SORT GT_DATA BY HROBJIDJT HROBJIDKG HROBJIDC1  HROBJIDC2 HROBJIDC3  HROBJIDC4 ."根据集团 、控股公司 、一级部门 、二级部门 、三级部门 、四级部门排序
    "根据不同纬度汇总 人员总数
   DATA: L_SUM  TYPE I .
   IF G_G1 EQ 'X' .   "根据集团纬度统计人数
      LOOP AT  GT_DATA  INTO GS_DATA .
        CLEAR :GS_OUT.            "
         L_SUM = L_SUM + GS_DATA-BMRS .
          AT END OF HROBJIDJT .   "以同一集团组织编码结束分块 统计
            GS_OUT-HROBJIDJT = GS_DATA-HROBJIDJT .
            READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDJT .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDJTNAME = <FS_HRP1000>-STEXT.
            ENDIF.
            GS_OUT-BMRS =  L_SUM .
            GS_OUT-ZBFB = L_SUM / Z_TOTAL * 100  .
            GS_OUT-ZBFB_1 = GS_OUT-ZBFB .
            CONCATENATE GS_OUT-ZBFB_1 '%' INTO GS_OUT-ZBFB_1.
            GS_OUT-GJRQ = P_DATE.
            APPEND GS_OUT TO GT_OUT.
            CLEAR :L_SUM.
          ENDAT.
      ENDLOOP.
   ENDIF .
   IF G_G2 EQ 'X' . "以控股公司组织编码纬度
      LOOP AT  GT_DATA  INTO GS_DATA .
         CLEAR :GS_OUT.        "
         L_SUM = L_SUM + GS_DATA-BMRS .
          AT END OF HROBJIDKG ."以同一控股公司组织编码结束分块统计
              GS_OUT-HROBJIDJT = GS_DATA-HROBJIDJT .
              GS_OUT-HROBJIDKG = GS_DATA-HROBJIDKG .
            READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDJT .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDJTNAME = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDKG .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDKGNAME = <FS_HRP1000>-STEXT.
            ENDIF.
            GS_OUT-BMRS =  L_SUM .
            GS_OUT-ZBFB = L_SUM / Z_TOTAL * 100  .
            GS_OUT-ZBFB_1 = GS_OUT-ZBFB .
             CONCATENATE GS_OUT-ZBFB_1 '%' INTO GS_OUT-ZBFB_1.
            GS_OUT-GJRQ = P_DATE.
             APPEND GS_OUT TO GT_OUT.
            CLEAR :L_SUM.
          ENDAT.
      ENDLOOP.
   ENDIF .

    IF G_G3 EQ 'X' . "以一级部门 纬度统计
          LOOP AT  GT_DATA  INTO GS_DATA .
          CLEAR :GS_OUT.           "以同一一级部门 组织编码结束分块统计
          L_SUM = L_SUM + GS_DATA-BMRS .
          AT END OF HROBJIDC1 ."
             GS_OUT-HROBJIDJT = GS_DATA-HROBJIDJT .
             GS_OUT-HROBJIDKG = GS_DATA-HROBJIDKG .
             GS_OUT-HROBJIDC1 = GS_DATA-HROBJIDC1 .
            READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDJT .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDJTNAME = <FS_HRP1000>-STEXT.
            ENDIF.
            READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDKG .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDKGNAME = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDC1 .
            IF SY-SUBRC = 0 .
               GS_OUT-C10RGEN = <FS_HRP1000>-STEXT.
            ENDIF.
            GS_OUT-BMRS =  L_SUM .
            GS_OUT-ZBFB = L_SUM / Z_TOTAL * 100  .
            GS_OUT-ZBFB_1 = GS_OUT-ZBFB .
            CONCATENATE GS_OUT-ZBFB_1 '%' INTO GS_OUT-ZBFB_1.
            GS_OUT-GJRQ = P_DATE.
             APPEND GS_OUT TO GT_OUT.
               CLEAR :L_SUM.
          ENDAT.
      ENDLOOP.
   ENDIF .

    IF G_G4 EQ 'X' .
      LOOP AT  GT_DATA  INTO GS_DATA .
          CLEAR :GS_OUT.
         L_SUM = L_SUM + GS_DATA-BMRS .
          AT END OF HROBJIDC2 . "以同一二级部门 组织编码结束分块统计
             GS_OUT-HROBJIDJT = GS_DATA-HROBJIDJT .
              GS_OUT-HROBJIDKG = GS_DATA-HROBJIDKG .
              GS_OUT-HROBJIDC1 = GS_DATA-HROBJIDC1 .
              GS_OUT-HROBJIDC2 = GS_DATA-HROBJIDC2.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDJT .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDJTNAME = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDKG .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDKGNAME = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDC1 .
            IF SY-SUBRC = 0 .
               GS_OUT-C10RGEN = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDC2 .
            IF SY-SUBRC = 0 .
               GS_OUT-C20RGEN = <FS_HRP1000>-STEXT.
            ENDIF.
            GS_OUT-BMRS =  L_SUM .
            GS_OUT-ZBFB = L_SUM / Z_TOTAL * 100  .
            GS_OUT-ZBFB_1 = GS_OUT-ZBFB .
            CONCATENATE GS_OUT-ZBFB_1 '%' INTO GS_OUT-ZBFB_1.
            GS_OUT-GJRQ = P_DATE.
             APPEND GS_OUT TO GT_OUT.
               CLEAR :L_SUM.
          ENDAT.
      ENDLOOP.
   ENDIF .

    IF G_G5 EQ 'X' .
    LOOP AT  GT_DATA  INTO GS_DATA .
         CLEAR :GS_OUT.
         L_SUM = L_SUM + GS_DATA-BMRS .
          AT END OF HROBJIDC3 . "以同一三级部门 组织编码结束分块统计
              GS_OUT-HROBJIDJT = GS_DATA-HROBJIDJT .
              GS_OUT-HROBJIDKG = GS_DATA-HROBJIDKG .
              GS_OUT-HROBJIDC1 = GS_DATA-HROBJIDC1 .
              GS_OUT-HROBJIDC2 = GS_DATA-HROBJIDC2.
              GS_OUT-HROBJIDC3 = GS_DATA-HROBJIDC3.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDJT .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDJTNAME = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDKG .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDKGNAME = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDC1 .
            IF SY-SUBRC = 0 .
               GS_OUT-C10RGEN = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDC2 .
            IF SY-SUBRC = 0 .
               GS_OUT-C20RGEN = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDC3 .
            IF SY-SUBRC = 0 .
               GS_OUT-C30RGEN = <FS_HRP1000>-STEXT.
            ENDIF.
            GS_OUT-BMRS =  L_SUM .
            GS_OUT-ZBFB = L_SUM / Z_TOTAL * 100  .
            GS_OUT-ZBFB_1 = GS_OUT-ZBFB .
           CONCATENATE GS_OUT-ZBFB_1 '%' INTO GS_OUT-ZBFB_1.
            GS_OUT-GJRQ = P_DATE.
             APPEND GS_OUT TO GT_OUT.
               CLEAR :L_SUM.
          ENDAT.
      ENDLOOP.
   ENDIF .

   IF G_G6 EQ 'X' .
     LOOP AT  GT_DATA  INTO GS_DATA .
         CLEAR :GS_OUT.
         L_SUM = L_SUM + GS_DATA-BMRS .
          AT END OF HROBJIDC4 ."以同一四级部门 组织编码结束分块统计
            GS_OUT-HROBJIDJT = GS_DATA-HROBJIDJT .
              GS_OUT-HROBJIDKG = GS_DATA-HROBJIDKG .
              GS_OUT-HROBJIDC1 = GS_DATA-HROBJIDC1 .
              GS_OUT-HROBJIDC2 = GS_DATA-HROBJIDC2.
              GS_OUT-HROBJIDC3 = GS_DATA-HROBJIDC3.
              GS_OUT-HROBJIDC4 = GS_DATA-HROBJIDC4.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDJT .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDJTNAME = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDKG .
            IF SY-SUBRC = 0 .
               GS_OUT-HROBJIDKGNAME = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDC1 .
            IF SY-SUBRC = 0 .
               GS_OUT-C10RGEN = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDC2 .
            IF SY-SUBRC = 0 .
               GS_OUT-C20RGEN = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDC3 .
            IF SY-SUBRC = 0 .
               GS_OUT-C30RGEN = <FS_HRP1000>-STEXT.
            ENDIF.
             READ TABLE  GT_HRP1000 ASSIGNING <FS_HRP1000> WITH KEY OBJID = GS_OUT-HROBJIDC4 .
            IF SY-SUBRC = 0 .
               GS_OUT-C40RGEN = <FS_HRP1000>-STEXT.
            ENDIF.
            GS_OUT-BMRS =  L_SUM .
            GS_OUT-ZBFB = L_SUM / Z_TOTAL * 100  .
            GS_OUT-ZBFB_1 = GS_OUT-ZBFB .
            CONCATENATE GS_OUT-ZBFB_1 '%' INTO GS_OUT-ZBFB_1.
            GS_OUT-GJRQ = P_DATE.
             APPEND GS_OUT TO GT_OUT.
            CLEAR :L_SUM.
          ENDAT.
      ENDLOOP.
   ENDIF .

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
 CLEAR :IT_FIELDCAT.

  PERFORM FILL_FIELDCAT USING 'GJRQ' '日期' SPACE SPACE  8 SPACE SPACE SPACE.


   PERFORM FILL_FIELDCAT USING 'HROBJIDJTNAME' '集团' SPACE SPACE  8 SPACE SPACE SPACE.


   PERFORM FILL_FIELDCAT USING 'HROBJIDKGNAME' '控股公司' SPACE SPACE  8 SPACE SPACE SPACE.


   PERFORM FILL_FIELDCAT USING 'C10RGEN' '一级部门' SPACE SPACE  8 SPACE SPACE SPACE.

   PERFORM FILL_FIELDCAT USING 'C20RGEN' '二级部门' SPACE SPACE  8 SPACE SPACE SPACE.

   PERFORM FILL_FIELDCAT USING 'C30RGEN' '三级部门' SPACE SPACE  8 SPACE SPACE SPACE.


  PERFORM FILL_FIELDCAT USING 'C40RGEN' '四级部门' SPACE SPACE  8 SPACE SPACE SPACE.


    PERFORM FILL_FIELDCAT USING 'BMRS' '人数' SPACE SPACE  8 SPACE SPACE SPACE.


    PERFORM FILL_FIELDCAT USING 'ZBFB_1' '百分比' SPACE SPACE  8 SPACE SPACE SPACE.

ENDFORM.
*&------------------------------IT_FIELDCAT---------------------------------------*
*&      Form  INITIAL_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIAL_LAYOUT .
  L_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  L_LAYOUT-BOX_FIELDNAME = 'BOX'.
  L_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  L_GRID_SETTINGS-EDT_CLL_CB ='X'.

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
 CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
    "  I_CALLBACK_TOP_OF_PAGE   = 'TOP-OF-PAGE'  "SEE FORM
  "     I_CALLBACK_USER_COMMAND  = 'ALV_USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      IT_FIELDCAT              = IT_FIELDCAT[]
      I_SAVE                   = 'X'
      I_GRID_SETTINGS          = L_GRID_SETTINGS
      IS_LAYOUT                = L_LAYOUT
      IS_VARIANT               = G_VARIANT
    TABLES
      T_OUTTAB                 = GT_OUT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
ENDFORM.

FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZHROM01_STATUS'.
ENDFORM.

FORM FILL_FIELDCAT USING FU_NAME
                         FU_TEXT
                         FU_TABLE
                         FU_FIELD
                         FU_OUTLEN
                         FU_KEY
                         FU_EDITMASK
                         FU_CHECKBOX.
  DATA:LW_FIELDCAT  LIKE LINE OF IT_FIELDCAT .


  LW_FIELDCAT-FIELDNAME = FU_NAME.
  LW_FIELDCAT-SELTEXT_L  = FU_TEXT.
  LW_FIELDCAT-REF_TABNAME = FU_TABLE.
  LW_FIELDCAT-REF_FIELDNAME = FU_FIELD.
  LW_FIELDCAT-OUTPUTLEN = FU_OUTLEN.
  LW_FIELDCAT-KEY       = FU_KEY.
  LW_FIELDCAT-EDIT_MASK = FU_EDITMASK.
  LW_FIELDCAT-CHECKBOX  = FU_CHECKBOX.
  APPEND LW_FIELDCAT TO IT_FIELDCAT .
ENDFORM.                    "fill_fieldcat
*&---------------------------------------------------------------------*
*&      Form  SELUP_ORG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0699   text
*      -->P_<LFS_ZZCJ>_ORGEH  text
*      <--P_GS_DATA_HROBJIDJT  text
*----------------------------------------------------------------------*
FORM SELUP_ORG  USING    P_SHORT TYPE SHORT_D
                         P_OBJ   TYPE ORGEH
                CHANGING R_OBJ   TYPE ORGEH.
DATA:ORG_TAB LIKE TABLE OF   QCAT_STRU WITH HEADER LINE.
DATA:OBJID TYPE HRP1000-OBJID .
OBJID =  P_OBJ .
CALL FUNCTION 'RHPH_STRUCTURE_READ'
  EXPORTING
    PLVAR                   = '01'
    OTYPE                   = 'O'
    OBJID                   =  OBJID
    WEGID                   = 'A002'
    BEGDA                   = P_DATE
    ENDDA                   = P_DATE
*   PUP_INFO                = 'X'
*   WITH_STEXT              = 'X'
*   TDEPTH                  = 0
  TABLES
    STRU_TAB                =  ORG_TAB
 EXCEPTIONS
   CATALOGUE_PROBLEM       = 1
   ROOT_NOT_FOUND          = 2
   WEGID_NOT_FOUND         = 3
  OTHERS                  = 4
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
 ELSE.
   LOOP AT ORG_TAB .
     IF ORG_TAB-SHORT = P_SHORT .
       R_OBJ = ORG_TAB-OBJID .
       EXIT.
       ENDIF.

   ENDLOOP.


ENDIF.

ENDFORM.
