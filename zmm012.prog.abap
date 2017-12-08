*&---------------------------------------------------------------------*
*& Report  ZMM010
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*&                  变 更 记 录                                         *
*&----------------------------------------------------------------------*
*& 日期       修改者         传输请求号     修改内容及原因
*&----------  ------------   ------------   ----------------------------*
*&2015-08-26  HANDYWLJ       ED1K903235     选择屏幕添加字段
REPORT ZMM010.

TYPE-POOLS: SLIS.
TABLES: MARD,MARA,MBEW,MARC.",
************************************************************************
*                       定义类型
************************************************************************

* 定义用与转换的类型
TYPES: BEGIN OF TY_OUT,
         INDEX(10) TYPE N, "序号
         MTART     TYPE MARA-MTART, "物料类型
         MATKL     TYPE MARA-MATKL, "物料组
         EXTWG     TYPE MARA-EXTWG,
         MATNR     TYPE MARA-MATNR, "物料编号
         MAKTX     TYPE MAKT-MAKTX, "规格说明
         MEINS     TYPE MARA-MEINS, "单位
         WERKS     TYPE MARD-WERKS, "工厂
         DISPO     TYPE MARC-DISPO,
         LGORT     TYPE MARD-LGORT, "仓码
         LOGGR     TYPE MARC-LOGGR, "仓管员代码
         LABST     TYPE MARD-LABST, "总数量
         NDNUM     TYPE MARD-LABST, "需求数量
         STPRS     TYPE P LENGTH 13 DECIMALS 2, "单价
         PEINH     TYPE PEINH,
         HSTPRS    TYPE P LENGTH 13 DECIMALS 2,
         HPEINH    TYPE PEINH,
         NUM1      TYPE MSEG-MENGE, "一个月以内
         STPRS1    TYPE P LENGTH 13 DECIMALS 2,
         NUM2      TYPE MSEG-MENGE, "1-2
         STPRS2    TYPE P LENGTH 13 DECIMALS 2,
         NUM3      TYPE MSEG-MENGE, "2-3
         STPRS3    TYPE P LENGTH 13 DECIMALS 2,
         NUM4      TYPE MSEG-MENGE, "3-6
         STPRS4    TYPE P LENGTH 13 DECIMALS 2,
         NUM5      TYPE MSEG-MENGE, "6-12
         STPRS5    TYPE P LENGTH 13 DECIMALS 2,
         NUM6      TYPE MSEG-MENGE, "12-24
         STPRS6    TYPE P LENGTH 13 DECIMALS 2,
         NUM7      TYPE MSEG-MENGE, ">12
         STPRS7    TYPE P LENGTH 13 DECIMALS 2,
         LTDAT     TYPE SY-DATUM, ""最后入库日期"
         LTNUM     TYPE MSEG-MENGE, ""最后入库数量"
         LUDAT     TYPE SY-DATUM, """最后耗用日期"
         LUNUM     TYPE MSEG-MENGE, ""最后耗用数量"
         DNUM      TYPE MSEG-MENGE, "呆滞数量
         DSTRPS    TYPE P LENGTH 13 DECIMALS 2, "呆滞金额
         CHECKBOX  TYPE C LENGTH 1,
         CELLCOLOR TYPE LVC_T_SCOL,
       END OF TY_OUT.

************************************************************************
*                    声明内表、工作区和全局变量
************************************************************************
DATA: TS_OUT TYPE STANDARD TABLE OF TY_OUT,
      TL_OUT LIKE LINE OF TS_OUT.
DATA: TS_MARD   LIKE TABLE OF MARD WITH HEADER LINE,
      TS_MARD_MSKA   LIKE TABLE OF MARD WITH HEADER LINE,
      TS_MARD_MCSD   LIKE TABLE OF MARD WITH HEADER LINE,
      TT_MARD   LIKE TABLE OF MARD WITH HEADER LINE,
      TS_MSKA   LIKE TABLE OF MSKA WITH HEADER LINE,
      TS_MSKA_01 lIKE TABLE OF MSKA WITH HEADER LINE,
      TS_MCSD   LIKE TABLE OF MCSD WITH HEADER LINE,
      TS_MCSD_01   LIKE TABLE OF MCSD WITH HEADER LINE,
      TS_MARA   LIKE TABLE OF MARA WITH HEADER LINE,
      TS_MAKT   LIKE TABLE OF MAKT WITH HEADER LINE,
      TS_MKPF   LIKE TABLE OF MKPF WITH HEADER LINE,
      TS_MSEG   LIKE TABLE OF MSEG WITH HEADER LINE,
      TT_MSEG   LIKE TABLE OF MSEG WITH HEADER LINE,
      TS_MBEW   LIKE TABLE OF MBEW WITH HEADER LINE,
      TS_CKMLCR LIKE TABLE OF CKMLCR WITH HEADER LINE,
      TS_MARC   LIKE TABLE OF MARC WITH HEADER LINE,
*      TS_ZTMM_QCKL LIKE TABLE OF ZTMM_QCKL WITH HEADER LINE,
      TS_ZMM012 LIKE TABLE OF ZMM012 WITH HEADER LINE,
      TS_MARDH  LIKE TABLE OF MARDH  WITH HEADER LINE,
      TS_MSKAH   LIKE TABLE OF MSKA WITH HEADER LINE,
      TS_MCSDH   LIKE TABLE OF MCSD WITH HEADER LINE.

DATA MINYEAR TYPE PYEAR.
DATA YEAR TYPE PYEAR.
DATA MONTH TYPE MONTH.

DATA P_YEAR TYPE PYEAR.
DATA P_MONTH TYPE MONTH.


DATA: DATE    LIKE SY-DATUM,
      DATU(8).


DATA: G_DATUM TYPE SY-DATUM, "最后一天日期
      G_MONTH TYPE N LENGTH 3, "月份
      G_DAY   TYPE N LENGTH 2. "天
DATA: I_DATE TYPE SY-DATUM.
** 使用 ALV
DATA: IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,    " ALV 标题内表
      WA_LAYOUT   TYPE SLIS_LAYOUT_ALV.        " ALV 格式
*      it_events   TYPE slis_t_event.           " ALV事件处理

FIELD-SYMBOLS <P_HSL>.                         " 定义指针


************************************************************************
*                    选择屏幕                                          *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-002.

SELECT-OPTIONS: S_WERKS FOR  MBEW-BWKEY OBLIGATORY,"   工厂
                S_LGORT FOR  MARD-LGORT ,"库存地点
                S_MATNR FOR  MARD-MATNR," 物料
                S_MTART FOR  MARA-MTART,
                S_MATKL FOR  MARA-MATKL,"物料组
                S_DISPO FOR  MARC-DISPO.
PARAMETERS:P_BUDAT TYPE MSEG-BUDAT_MKPF."过账日期
SELECTION-SCREEN END OF BLOCK BLK2.

************************************************************************
*                    初始化事件                                        *
************************************************************************
INITIALIZATION.


************************************************************************
*                    选择屏幕事件                                      *
************************************************************************
AT SELECTION-SCREEN.





************************************************************************
*                   START-OF-SELECTION                                *
************************************************************************
START-OF-SELECTION.


  PERFORM  FRM_GETDATA.
  PERFORM  FRM_DEALDATA.

************************************************************************
*                   END-OF-SELECTION                                   *
************************************************************************
END-OF-SELECTION.
* using alv to display report
  PERFORM FRM_ALV.

*&---------------------------------------------------------------------*
*&      Form  FRM_GETDATA
*&---------------------------------------------------------------------*
*  功能描述：按指定条件从数据库表中取出数据，并传入相应的内表或工作区
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM FRM_GETDATA.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_ZMM012 FROM ZMM012.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE TT_MARD
     FROM MARD
    WHERE WERKS IN S_WERKS AND
          LGORT IN S_LGORT AND
          MATNR IN S_MATNR .
  LOOP AT TT_MARD.
    IF TT_MARD-LABST - TT_MARD-INSME = 0.
    ELSE.
      APPEND TT_MARD TO TS_MARD.
    ENDIF.
  ENDLOOP.



  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MSKA
  FROM MSKA
 WHERE WERKS IN S_WERKS AND
       LGORT IN S_LGORT AND
       MATNR IN S_MATNR .
  TS_MSKA_01[] = TS_MSKA[].
  SORT TS_MSKA_01 BY MATNR WERKS LGORT .
  DELETE ADJACENT DUPLICATES FROM TS_MSKA_01 COMPARING MATNR WERKS LGORT .
  MOVE-CORRESPONDING  TS_MSKA_01[] TO TS_MARD_MSKA[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MCSD
 FROM MCSD
WHERE WERKS IN S_WERKS AND
      LGORT IN S_LGORT AND
      MATNR IN S_MATNR .

  TS_MCSD_01[] = TS_MCSD[].
  SORT TS_MCSD_01 BY MATNR WERKS LGORT .
  DELETE ADJACENT DUPLICATES FROM TS_MCSD_01 COMPARING MATNR WERKS LGORT .
  MOVE-CORRESPONDING  TS_MCSD_01[] TO TS_MARD_MCSD[].

  APPEND LINES OF TS_MARD_MCSD[] TO TS_MARD[].
  APPEND LINES OF TS_MARD_MSKA[] TO TS_MARD[].
  SORT TS_MARD BY MATNR WERKS LGORT.
  DELETE ADJACENT DUPLICATES FROM TS_MARD COMPARING MATNR WERKS LGORT .

  IF TS_MARD[] IS INITIAL.
    MESSAGE '没有数据' TYPE 'E' DISPLAY LIKE 'I'.
  ENDIF.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MARA
    FROM MARA FOR ALL ENTRIES IN TS_MARD
    WHERE MATNR = TS_MARD-MATNR
      AND MTART IN S_MTART
      AND MATKL IN S_MATKL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MARC
    FROM MARC FOR ALL ENTRIES IN TS_MARD
    WHERE MATNR = TS_MARD-MATNR
    AND   WERKS = TS_MARD-WERKS
    AND   DISPO IN S_DISPO.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MAKT
    FROM MAKT FOR ALL ENTRIES IN TS_MARD
    WHERE MATNR = TS_MARD-MATNR
    AND SPRAS = SY-LANGU.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MKPF
    FROM MKPF .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MSEG
    FROM MSEG FOR ALL ENTRIES IN TS_MARD
    WHERE MATNR = TS_MARD-MATNR
    AND   WERKS = TS_MARD-WERKS
    AND   LGORT = TS_MARD-LGORT.
  REFRESH TT_MSEG.
  APPEND LINES OF TS_MSEG TO TT_MSEG.
  SORT TS_MSEG BY MATNR WERKS LGORT  CPUDT_MKPF   DESCENDING .
  SORT TT_MSEG BY MATNR WERKS LGORT  BUDAT_MKPF   .


*  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_ZTMM_QCKL
*    FROM
.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MBEW
    FROM MBEW FOR ALL ENTRIES IN TS_MARD
    WHERE MATNR = TS_MARD-MATNR AND
          BWKEY = TS_MARD-WERKS.

  SORT TS_MBEW BY MATNR BWKEY.

  IF TS_MBEW[] IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_CKMLCR
      FROM CKMLCR FOR ALL ENTRIES IN TS_MBEW
      WHERE KALNR = TS_MBEW-KALN1
      AND   CURTP = '30'.
    MINYEAR = SY-DATUM+0(4).
    IF TS_CKMLCR[] IS NOT INITIAL.
      SORT TS_CKMLCR BY BDATJ.
      READ TABLE TS_CKMLCR INDEX 1.
      MINYEAR = TS_CKMLCR-BDATJ - 1.
    ENDIF.

  ENDIF.

 " ADD BY IT02 2015/12/24 BEGIN
  IF P_BUDAT IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MARDH
    FROM MARDH
    WHERE WERKS IN S_WERKS AND
         LGORT IN S_LGORT AND
         MATNR IN S_MATNR AND
         LFGJA = P_BUDAT+0(4)  AND
         LFMON = P_BUDAT+4(2) .
   SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MSKAH
    FROM MSKAH
    WHERE WERKS IN S_WERKS AND
       LGORT IN S_LGORT AND
       MATNR IN S_MATNR AND
      LFGJA = P_BUDAT+0(4)  AND
         LFMON = P_BUDAT+4(2) .
   SELECT * INTO CORRESPONDING FIELDS OF TABLE TS_MCSDH
    FROM MCSDH
    WHERE WERKS IN S_WERKS AND
      LGORT IN S_LGORT AND
      MATNR IN S_MATNR AND
       LFGJA = P_BUDAT+0(4)  AND
         LFMON = P_BUDAT+4(2) .
  ENDIF.
 " ADD BY IT02 2015/12/24 END


ENDFORM.                    " FRM_GETDATA
*&---------------------------------------------------------------------*
*&      Form  FRM_DEALDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DEALDATA .
  DATA INDEX(10) TYPE N VALUE 0.
  DATA TS_MDPS LIKE TABLE OF MDPS WITH HEADER LINE.
  DATA BDATE LIKE SY-DATUM.
  DATA LDATE LIKE SY-DATUM.
  DATA FLAG TYPE C.
  DATA UKURS TYPE UKURS_CURR.
  DATA:P_BUDAT2 TYPE SY-DATUM.
  P_BUDAT2 = P_BUDAT.

*     CALL FUNCTION 'READ_EXCHANGE_RATE'
*      EXPORTING
**       CLIENT                  = SY-MANDT
*        DATE                    = SY-DATUM
*        FOREIGN_CURRENCY        = 'CNY'
*        LOCAL_CURRENCY          = 'HKD'
*        TYPE_OF_RATE            = 'M'
**       EXACT_DATE              = ' '
*     IMPORTING
*       EXCHANGE_RATE           =  UKURS
**       FOREIGN_FACTOR          =
**       LOCAL_FACTOR            =
**       VALID_FROM_DATE         =
**       DERIVED_RATE_TYPE       =
**       FIXED_RATE              =
**       OLDEST_RATE_FROM        =
*     EXCEPTIONS
*       NO_RATE_FOUND           = 1
*       NO_FACTORS_FOUND        = 2
**       NO_SPREAD_FOUND         = 3
**       DERIVED_2_TIMES         = 4
**       OVERFLOW                = 5
**       ZERO_RATE               = 6
**       OTHERS                  = 7
* .
  LOOP AT TS_MARD.
    CLEAR FLAG.
    CLEAR TL_OUT.
    CLEAR TS_MARA.
    READ TABLE TS_MARA WITH KEY MATNR = TS_MARD-MATNR.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
    TL_OUT-MTART = TS_MARA-MTART.
    TL_OUT-MATKL = TS_MARA-MATKL.
    TL_OUT-EXTWG = TS_MARA-EXTWG.
    TL_OUT-MATNR = TS_MARD-MATNR.

    CLEAR TS_MARC.
    READ TABLE TS_MARC WITH KEY MATNR = TS_MARD-MATNR
                                WERKS = TS_MARD-WERKS.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.
    TL_OUT-LOGGR = TS_MARC-LOGGR.
    TL_OUT-DISPO = TS_MARC-DISPO.
    CLEAR TS_MAKT.
    READ TABLE TS_MAKT WITH KEY MATNR = TS_MARD-MATNR.
    TL_OUT-MAKTX = TS_MAKT-MAKTX.

    TL_OUT-MEINS = TS_MARA-MEINS.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        INPUT  = TL_OUT-MEINS
*       LANGUAGE             = SY-LANGU
      IMPORTING
*       LONG_TEXT            =
        OUTPUT = TL_OUT-MEINS
*       SHORT_TEXT           =
*           EXCEPTIONS
*       UNIT_NOT_FOUND       = 1
*       OTHERS = 2
      .
    TL_OUT-WERKS = TS_MARD-WERKS.
    TL_OUT-LGORT = TS_MARD-LGORT.
    IF P_BUDAT2 IS INITIAL.
      TL_OUT-LABST = TS_MARD-LABST + TS_MARD-INSME.

    CLEAR TS_MSKA.
    LOOP AT TS_MSKA WHERE WERKS = TS_MARD-WERKS
                           AND  MATNR = TS_MARD-MATNR
                           AND  LGORT = TS_MARD-LGORT.
       TL_OUT-LABST = TL_OUT-LABST + TS_MSKA-KALAB + TS_MSKA-KAINS.
    ENDLOOP.
*    READ TABLE TS_MSKA WITH KEY WERKS = TS_MARD-WERKS
*                                MATNR = TS_MARD-MATNR
*                                LGORT = TS_MARD-LGORT.
*    TL_OUT-LABST = TL_OUT-LABST + TS_MSKA-KALAB + TS_MSKA-KAINS.

    CLEAR TS_MCSD.

    LOOP AT TS_MCSD WHERE WERKS = TS_MARD-WERKS
                          AND  MATNR = TS_MARD-MATNR
                          AND  LGORT = TS_MARD-LGORT.
       TL_OUT-LABST = TL_OUT-LABST + TS_MCSD-SDLAB + TS_MCSD-SDINS.
   ENDLOOP.
*    READ TABLE TS_MCSD WITH KEY WERKS = TS_MARD-WERKS
*                                MATNR = TS_MARD-MATNR
*                                LGORT = TS_MARD-LGORT.
*    TL_OUT-LABST = TL_OUT-LABST + TS_MCSD-SDLAB + TS_MCSD-SDINS.
    ENDIF.


    " ADD BY IT02 2015/12/24 BEGIN
    IF P_BUDAT2 IS NOT INITIAL.
      READ TABLE TS_MARDH WITH KEY WERKS = TS_MARD-WERKS
                                MATNR = TS_MARD-MATNR
                                LGORT = TS_MARD-LGORT
                                LFGJA = P_BUDAT+0(4)
                                LFMON = P_BUDAT+4(2).
      IF SY-SUBRC = 0.
         CLEAR :TL_OUT-LABST.
         TL_OUT-LABST = TS_MARDH-LABST + TS_MARDH-INSME.
      ENDIF.
      CLEAR TS_MSKAH.
    LOOP AT TS_MSKAH WHERE WERKS = TS_MARD-WERKS
                           AND  MATNR = TS_MARD-MATNR
                           AND  LGORT = TS_MARD-LGORT.
       TL_OUT-LABST = TL_OUT-LABST + TS_MSKAH-KALAB + TS_MSKAH-KAINS.
    ENDLOOP.

    CLEAR TS_MCSDH.

    LOOP AT TS_MCSDH WHERE WERKS = TS_MARD-WERKS
                          AND  MATNR = TS_MARD-MATNR
                          AND  LGORT = TS_MARD-LGORT.
       TL_OUT-LABST = TL_OUT-LABST + TS_MCSDH-SDLAB + TS_MCSDH-SDINS.
   ENDLOOP.

    ENDIF.
    " ADD BY IT02 2015/12/24 END
    CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
      EXPORTING
*       PLSCN =
        MATNR = TS_MARD-MATNR
        WERKS = TS_MARD-WERKS
*       BERID =
*       ERGBZ =
*       AFIBZ =
*       INPER =
*       DISPLAY_LIST_MDPSX             =
*       DISPLAY_LIST_MDEZX             =
*       DISPLAY_LIST_MDSUX             =
*       NOBUF =
*       PLAUF =
*       I_VRFWE                        =
*       IS_SFILT                       =
*       IS_AFILT                       =
*    IMPORTING
*       E_MT61D                        =
*       E_MDKP                         =
*       E_CM61M                        =
*       E_MDSTA                        =
*       E_ERGBZ                        =
      TABLES
        MDPSX = TS_MDPS
*       MDEZX =
*       MDSUX =
*    EXCEPTIONS
*       MATERIAL_PLANT_NOT_FOUND       = 1
*       PLANT_NOT_FOUND                = 2
*       OTHERS                         = 3
      .
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    LOOP AT TS_MDPS WHERE PLUMI = '-'.
      TL_OUT-NDNUM = TL_OUT-NDNUM + TS_MDPS-MNG01.
    ENDLOOP.
*    IF TL_OUT-WERKS NE '1100'.
*      CLEAR TS_MBEW.
*      READ TABLE TS_MBEW WITH KEY MATNR = TS_MARD-MATNR BWKEY = TS_MARD-WERKS.
*
*      TL_OUT-STPRS =  TS_MBEW-VERPR.
*      TL_OUT-PEINH =  TS_MBEW-PEINH.
*
*      P_YEAR = SY-DATUM+0(4).
*      P_MONTH = SY-DATUM+4(2).
*      PERFORM GET_PVPRS USING  P_YEAR P_MONTH TS_MBEW-KALN1
*                        CHANGING TL_OUT-HSTPRS TL_OUT-HPEINH.
*      "TL_OUT-HPEINH = TL_OUT-PEINH.
*
*    ELSE.
      CLEAR TS_MBEW.
      READ TABLE TS_MBEW WITH KEY MATNR = TS_MARD-MATNR BWKEY = TS_MARD-WERKS BINARY SEARCH.
      TL_OUT-HSTPRS =  TS_MBEW-VERPR.
      TL_OUT-HPEINH =  TS_MBEW-PEINH.
      TL_OUT-STPRS =  TS_MBEW-STPRS.
      TL_OUT-PEINH =  TS_MBEW-PEINH.
*    ENDIF.
    LOOP AT TS_MSEG WHERE  MATNR = TS_MARD-MATNR
                      AND    WERKS = TS_MARD-WERKS
                      AND    LGORT = TS_MARD-LGORT
                      AND    SHKZG = 'S'.
*                      AND    BWART NE '561'.
      IF P_BUDAT IS INITIAL.
        P_BUDAT = SY-DATUM.
      ENDIF.

      IF  P_BUDAT - TS_MSEG-BUDAT_MKPF >= 0 AND P_BUDAT - TS_MSEG-BUDAT_MKPF < 30.

        TL_OUT-NUM1 = TL_OUT-NUM1 + TS_MSEG-MENGE.

      ELSEIF  P_BUDAT - TS_MSEG-BUDAT_MKPF >= 30 AND P_BUDAT - TS_MSEG-BUDAT_MKPF < 60.

        TL_OUT-NUM2 = TL_OUT-NUM2 + TS_MSEG-MENGE.

      ELSEIF  P_BUDAT - TS_MSEG-BUDAT_MKPF >= 60 AND P_BUDAT - TS_MSEG-BUDAT_MKPF < 90.

        TL_OUT-NUM3 = TL_OUT-NUM3 + TS_MSEG-MENGE.

      ELSEIF  P_BUDAT - TS_MSEG-BUDAT_MKPF >= 90 AND P_BUDAT - TS_MSEG-BUDAT_MKPF < 180.

        TL_OUT-NUM4 = TL_OUT-NUM4 + TS_MSEG-MENGE.

      ELSEIF  P_BUDAT - TS_MSEG-BUDAT_MKPF >= 180 AND P_BUDAT - TS_MSEG-BUDAT_MKPF < 365.

        TL_OUT-NUM5 = TL_OUT-NUM5 + TS_MSEG-MENGE.

      ELSEIF  P_BUDAT - TS_MSEG-BUDAT_MKPF >= 365 AND P_BUDAT - TS_MSEG-BUDAT_MKPF < 730.

        TL_OUT-NUM6 = TL_OUT-NUM6 + TS_MSEG-MENGE.

      ELSEIF  P_BUDAT - TS_MSEG-BUDAT_MKPF >= 730 .

        TL_OUT-NUM7 = TL_OUT-NUM7 + TS_MSEG-MENGE.

      ENDIF.

    ENDLOOP.

*    LOOP AT TS_ZTMM_QCKL WHERE  MATNR = TS_MARD-MATNR
*                      AND    BWKEY = TS_MARD-WERKS
*                      AND    LGORT = TS_MARD-LGORT.
*      IF  SY-DATUM - TS_ZTMM_QCKL-BUDAT >= 0 AND SY-DATUM - TS_ZTMM_QCKL-BUDAT < 30.
*
*        TL_OUT-NUM1 = TL_OUT-NUM1 + TS_ZTMM_QCKL-MENGE.
*
*      ELSEIF  SY-DATUM - TS_ZTMM_QCKL-BUDAT >= 30 AND SY-DATUM - TS_ZTMM_QCKL-BUDAT < 60.
*
*        TL_OUT-NUM2 = TL_OUT-NUM2 + TS_ZTMM_QCKL-MENGE.
*
*      ELSEIF  SY-DATUM - TS_ZTMM_QCKL-BUDAT >= 60 AND SY-DATUM - TS_ZTMM_QCKL-BUDAT < 90.
*
*        TL_OUT-NUM3 = TL_OUT-NUM3 + TS_ZTMM_QCKL-MENGE.
*
*      ELSEIF  SY-DATUM - TS_ZTMM_QCKL-BUDAT >= 90 AND SY-DATUM - TS_ZTMM_QCKL-BUDAT < 180.
*
*        TL_OUT-NUM4 = TL_OUT-NUM4 + TS_ZTMM_QCKL-MENGE.
*
*      ELSEIF  SY-DATUM - TS_ZTMM_QCKL-BUDAT >= 180 AND SY-DATUM - TS_ZTMM_QCKL-BUDAT < 365.
*
*        TL_OUT-NUM5 = TL_OUT-NUM5 + TS_ZTMM_QCKL-MENGE.
*
*      ELSEIF  SY-DATUM - TS_ZTMM_QCKL-BUDAT >= 365 AND SY-DATUM - TS_ZTMM_QCKL-BUDAT < 730.
*
*        TL_OUT-NUM6 = TL_OUT-NUM6 + TS_ZTMM_QCKL-MENGE.
*
*      ELSEIF  SY-DATUM - TS_ZTMM_QCKL-BUDAT >= 730 .
*
*        TL_OUT-NUM7 = TL_OUT-NUM7 + TS_ZTMM_QCKL-MENGE.
*
*      ENDIF.
*
*    ENDLOOP.

    IF TL_OUT-NUM1 > TL_OUT-LABST.
      TL_OUT-NUM1 = TL_OUT-LABST.
    ENDIF.

    IF TL_OUT-NUM2 > TL_OUT-LABST - TL_OUT-NUM1.
      TL_OUT-NUM2 = TL_OUT-LABST - TL_OUT-NUM1.
    ENDIF.

    IF TL_OUT-NUM3 > TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 .
      TL_OUT-NUM3 = TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2.
    ENDIF.

    IF TL_OUT-NUM4 > TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 .
      TL_OUT-NUM4 = TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 ..
    ENDIF.

    IF TL_OUT-NUM5 > TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 - TL_OUT-NUM4 .
      TL_OUT-NUM5 = TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 - TL_OUT-NUM4.
    ENDIF.

    IF TL_OUT-NUM6 > TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 - TL_OUT-NUM4 - TL_OUT-NUM5.
      TL_OUT-NUM6 = TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 - TL_OUT-NUM4 - TL_OUT-NUM5.
    ENDIF.

    IF TL_OUT-NUM7 > TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 - TL_OUT-NUM4 - TL_OUT-NUM5 - TL_OUT-NUM6.
      TL_OUT-NUM7 = TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 - TL_OUT-NUM4 - TL_OUT-NUM5 - TL_OUT-NUM6.
    ENDIF.
*    IF TL_OUT-WERKS = '1100' AND TL_OUT-HPEINH NE 0.
*      TL_OUT-STPRS1 = TL_OUT-NUM1 * TL_OUT-HSTPRS / TL_OUT-HPEINH.
*      TL_OUT-STPRS2 = TL_OUT-NUM2 * TL_OUT-HSTPRS / TL_OUT-HPEINH.
*      TL_OUT-STPRS3 = TL_OUT-NUM3 * TL_OUT-HSTPRS / TL_OUT-HPEINH.
*      TL_OUT-STPRS4 = TL_OUT-NUM4 * TL_OUT-HSTPRS / TL_OUT-HPEINH.
*      TL_OUT-STPRS5 = TL_OUT-NUM5 * TL_OUT-HSTPRS / TL_OUT-HPEINH.
*      TL_OUT-STPRS6 = TL_OUT-NUM6 * TL_OUT-HSTPRS / TL_OUT-HPEINH.
*      TL_OUT-STPRS7 = TL_OUT-NUM7 * TL_OUT-HSTPRS / TL_OUT-HPEINH.
*    ELSEIF TL_OUT-PEINH NE 0.
      TL_OUT-STPRS1 = TL_OUT-NUM1 * TL_OUT-STPRS / TL_OUT-PEINH.
      TL_OUT-STPRS2 = TL_OUT-NUM2 * TL_OUT-STPRS / TL_OUT-PEINH.
      TL_OUT-STPRS3 = TL_OUT-NUM3 * TL_OUT-STPRS / TL_OUT-PEINH.
      TL_OUT-STPRS4 = TL_OUT-NUM4 * TL_OUT-STPRS / TL_OUT-PEINH.
      TL_OUT-STPRS5 = TL_OUT-NUM5 * TL_OUT-STPRS / TL_OUT-PEINH.
      TL_OUT-STPRS6 = TL_OUT-NUM6 * TL_OUT-STPRS / TL_OUT-PEINH.
      TL_OUT-STPRS7 = TL_OUT-NUM7 * TL_OUT-STPRS / TL_OUT-PEINH.
*    ENDIF.

*   BDATE = SY-DATUM - 30.
*   LDATE = SY-DATUM.
*   LOOP AT TS_MKPF WHERE BUDAT > BDATE AND BUDAT <= LDATE.
*     LOOP AT TS_MSEG WHERE  MATNR = TS_MARD-MATNR
*                     AND    MBLNR = TS_MKPF-MBLNR
*                     AND    MJAHR = TS_MKPF-MJAHR
*                     AND    WERKS = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT
*                     AND    SHKZG = 'S'.
*       TL_OUT-NUM1 = TL_OUT-NUM1 + TS_MSEG-MENGE.
*     ENDLOOP.
*   ENDLOOP.
*   LOOP AT TS_ZTMM_QCKL WHERE BUDAT > BDATE
*                     AND BUDAT <= SY-DATUM
*                     AND   MATNR = TS_MARD-MATNR
*                     AND    BWKEY = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT.
*        TL_OUT-NUM1 = TL_OUT-NUM1 + TS_ZTMM_QCKL-MENGE.
*   ENDLOOP.
*   TL_OUT-STPRS1 = TL_OUT-NUM1 * TL_OUT-STPRS.
*   IF TL_OUT-LABST =< TL_OUT-NUM1.
*     FLAG = 'X'.
*   ENDIF.
**
*
*   IF FLAG = ''.
*   BDATE = SY-DATUM - 60.
*   LDATE = SY-DATUM - 30.
*   LOOP AT TS_MKPF WHERE BUDAT > BDATE AND BUDAT <= LDATE.
*     LOOP AT TS_MSEG WHERE  MATNR = TS_MARD-MATNR
*                     AND    MBLNR = TS_MKPF-MBLNR
*                     AND    MJAHR = TS_MKPF-MJAHR
*                     AND    WERKS = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT
*                     AND    SHKZG = 'S'.
*       TL_OUT-NUM2 = TL_OUT-NUM2 + TS_MSEG-MENGE.
*     ENDLOOP.
*   ENDLOOP.
*   LOOP AT TS_ZTMM_QCKL WHERE BUDAT > BDATE
*                     AND BUDAT <= SY-DATUM
*                     AND   MATNR = TS_MARD-MATNR
*                     AND    BWKEY = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT.
*        TL_OUT-NUM2 = TL_OUT-NUM2 + TS_ZTMM_QCKL-MENGE.
*   ENDLOOP.
*  TL_OUT-STPRS2 = TL_OUT-NUM2 * TL_OUT-STPRS.
*   IF TL_OUT-LABST - TL_OUT-NUM1 =< TL_OUT-NUM2.
*     FLAG = 'X'.
*   ENDIF.
*   ENDIF.
*
*   IF FLAG = ''.
*   BDATE = SY-DATUM - 90.
*   LDATE = SY-DATUM - 60.
*   LOOP AT TS_MKPF WHERE BUDAT > BDATE AND BUDAT <= LDATE.
*     LOOP AT TS_MSEG WHERE  MATNR = TS_MARD-MATNR
*                     AND    MBLNR = TS_MKPF-MBLNR
*                     AND    MJAHR = TS_MKPF-MJAHR
*                     AND    WERKS = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT
*                     AND    SHKZG = 'S'.
*       TL_OUT-NUM3 = TL_OUT-NUM3 + TS_MSEG-MENGE.
*     ENDLOOP.
*   ENDLOOP.
*   LOOP AT TS_ZTMM_QCKL WHERE BUDAT > BDATE
*                     AND BUDAT <= SY-DATUM
*                     AND   MATNR = TS_MARD-MATNR
*                     AND    BWKEY = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT.
*        TL_OUT-NUM3 = TL_OUT-NUM3 + TS_ZTMM_QCKL-MENGE.
*   ENDLOOP.
*  TL_OUT-STPRS3 = TL_OUT-NUM3 * TL_OUT-STPRS.
*   IF TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 =< TL_OUT-NUM3.
*     FLAG = 'X'.
*   ENDIF.
*   ENDIF.
*
*   IF FLAG = ''.
*     BDATE = SY-DATUM - 180.
*   LDATE = SY-DATUM - 90.
*   LOOP AT TS_MKPF WHERE BUDAT > BDATE AND BUDAT <= LDATE.
*     LOOP AT TS_MSEG WHERE  MATNR = TS_MARD-MATNR
*                     AND    MBLNR = TS_MKPF-MBLNR
*                     AND    MJAHR = TS_MKPF-MJAHR
*                     AND    WERKS = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT
*                     AND    SHKZG = 'S'.
*       TL_OUT-NUM4 = TL_OUT-NUM4 + TS_MSEG-MENGE.
*     ENDLOOP.
*   ENDLOOP.
*   LOOP AT TS_ZTMM_QCKL WHERE BUDAT > BDATE
*                     AND BUDAT <= SY-DATUM
*                     AND   MATNR = TS_MARD-MATNR
*                     AND    BWKEY = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT.
*        TL_OUT-NUM4 = TL_OUT-NUM4 + TS_ZTMM_QCKL-MENGE.
*   ENDLOOP.
*  TL_OUT-STPRS4 = TL_OUT-NUM4 * TL_OUT-STPRS.
*   IF TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 =< TL_OUT-NUM4.
*     FLAG = 'X'.
*   ENDIF.
*   ENDIF.
*
*   IF FLAG = ''.
*     BDATE = SY-DATUM - 365.
*   LDATE = SY-DATUM - 180.
*   LOOP AT TS_MKPF WHERE BUDAT > BDATE AND BUDAT <= LDATE.
*     LOOP AT TS_MSEG WHERE  MATNR = TS_MARD-MATNR
*                     AND    MBLNR = TS_MKPF-MBLNR
*                     AND    MJAHR = TS_MKPF-MJAHR
*                     AND    WERKS = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT
*                     AND    SHKZG = 'S'.
*       TL_OUT-NUM5 = TL_OUT-NUM5 + TS_MSEG-MENGE.
*     ENDLOOP.
*   ENDLOOP.
*   LOOP AT TS_ZTMM_QCKL WHERE BUDAT > BDATE
*                     AND BUDAT <= SY-DATUM
*                     AND   MATNR = TS_MARD-MATNR
*                     AND    BWKEY = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT.
*        TL_OUT-NUM5 = TL_OUT-NUM5 + TS_ZTMM_QCKL-MENGE.
*   ENDLOOP.
*  TL_OUT-STPRS5 = TL_OUT-NUM5 * TL_OUT-STPRS.
*   IF TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 - TL_OUT-NUM4 =< TL_OUT-NUM5.
*     FLAG = 'X'.
*   ENDIF.
*   ENDIF.
*
*   IF FLAG = ''.
*     BDATE = SY-DATUM - 730.
*   LDATE = SY-DATUM - 365.
*   LOOP AT TS_MKPF WHERE BUDAT > BDATE AND BUDAT <= LDATE.
*     LOOP AT TS_MSEG WHERE  MATNR = TS_MARD-MATNR
*                     AND    MBLNR = TS_MKPF-MBLNR
*                     AND    MJAHR = TS_MKPF-MJAHR
*                     AND    WERKS = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT
*                     AND    SHKZG = 'S'.
*       TL_OUT-NUM6 = TL_OUT-NUM6 + TS_MSEG-MENGE.
*     ENDLOOP.
*   ENDLOOP.
*   LOOP AT TS_ZTMM_QCKL WHERE BUDAT > BDATE
*                     AND BUDAT <= SY-DATUM
*                     AND   MATNR = TS_MARD-MATNR
*                     AND    BWKEY = TS_MARD-WERKS
*                     AND    LGORT = TS_MARD-LGORT.
*        TL_OUT-NUM6 = TL_OUT-NUM6 + TS_ZTMM_QCKL-MENGE.
*   ENDLOOP.
*  TL_OUT-STPRS6 = TL_OUT-NUM6 * TL_OUT-STPRS.
*   IF TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3 - TL_OUT-NUM4 - TL_OUT-NUM5 =< TL_OUT-NUM6.
*     FLAG = 'X'.
*   ENDIF.
*   ENDIF.
*
*  IF FLAG = ''.
*  TL_OUT-NUM7 =  TL_OUT-LABST - TL_OUT-NUM1 - TL_OUT-NUM2 - TL_OUT-NUM3
*                 - TL_OUT-NUM4 - TL_OUT-NUM5 - TL_OUT-NUM6.
*  TL_OUT-STPRS7 = TL_OUT-NUM7 * TL_OUT-STPRS.
*  ENDIF.
*
    CLEAR TS_MSEG.
    CLEAR TT_MSEG.
    CLEAR TS_ZMM012.
    READ TABLE TS_ZMM012 WITH KEY WERKS = TS_MARD-WERKS
                                  LGORT = TS_MARD-LGORT.
    IF SY-SUBRC EQ 0.
*      IF TL_OUT-MTART = 'ZFRT' OR TL_OUT-MTART =  'ZHLB'.
*        LOOP AT TT_MSEG WHERE      SHKZG = 'S'  AND
*                                    BWART = '101'  AND
*                                    MATNR = TS_MARD-MATNR  AND
*                                    WERKS = TS_MARD-WERKS  AND
*                                    LGORT = TS_MARD-LGORT.
*        IF P_BUDAT2 IS NOT INITIAL.
*            IF TT_MSEG-BUDAT_MKPF > P_BUDAT2.
*                 EXIT.
*            ENDIF.
*        ENDIF..
*        ENDLOOP.
*      ELSE.
        LOOP AT TT_MSEG WHERE      SHKZG = 'S' AND
                                   ( VGART_MKPF = 'WE' OR VGART_MKPF = 'WQ'  OR VGART_MKPF = 'WR'   OR VGART_MKPF = 'WF'  ) AND
                                    MATNR = TS_MARD-MATNR AND
                                    WERKS = TS_MARD-WERKS AND
                                    LGORT = TS_MARD-LGORT.
         IF P_BUDAT2 IS NOT INITIAL.
            IF TT_MSEG-BUDAT_MKPF > P_BUDAT2.
                 EXIT.
            ENDIF.
         ENDIF..
        ENDLOOP.
   "   ENDIF.
    ELSE.
      LOOP AT TT_MSEG WHERE      SHKZG = 'S' AND
                                  MATNR = TS_MARD-MATNR AND
                                  WERKS = TS_MARD-WERKS AND
                                  LGORT = TS_MARD-LGORT.
     IF P_BUDAT2 IS NOT INITIAL.
          IF TT_MSEG-BUDAT_MKPF > P_BUDAT2.
               EXIT.
          ENDIF.
     ENDIF.
      ENDLOOP.
    ENDIF.
    TL_OUT-LTDAT = TT_MSEG-BUDAT_MKPF."最后入库日期"
    TL_OUT-LTNUM =  TT_MSEG-MENGE.
    IF TL_OUT-LTDAT IS INITIAL.
*      CLEAR TS_ZTMM_QCKL.
*      READ TABLE TS_ZTMM_QCKL WITH KEY MATNR = TS_MARD-MATNR
*                                       BWKEY = TS_MARD-WERKS
*                                       LGORT = TS_MARD-LGORT.
*      TL_OUT-LTDAT =  TS_ZTMM_QCKL-BUDAT.
*      TL_OUT-LTNUM =  TS_ZTMM_QCKL-MENGE.
    ENDIF.

*  CLEAR TS_MKPF.
*  READ TABLE TS_MKPF WITH KEY BUDAT = SY-DATUM.
*  CLEAR TS_MSEG.
*  READ TABLE TS_MSEG WITH KEY MATNR = TS_MARD-MATNR
*                              MBLNR = TS_MKPF-MBLNR
*                              MJAHR = TS_MKPF-MJAHR
*                              WERKS = TS_MARD-WERKS
*                              LGORT = TS_MARD-LGORT
*                              SHKZG = 'S'.
*  TL_OUT-LTNUM = TS_MSEG-MENGE.""最后入库数量"

    CLEAR TT_MSEG.
    LOOP AT  TT_MSEG WHERE   SHKZG = 'H'  AND
*                       FNR <> '' AND
                         VGART_MKPF = 'WA' AND
*                         AUFNR <> '' S    AND
                         MATNR = TS_MARD-MATNR  AND
                         WERKS = TS_MARD-WERKS  AND
                         LGORT = TS_MARD-LGORT.
      IF P_BUDAT2 IS NOT INITIAL.
          IF TT_MSEG-BUDAT_MKPF > P_BUDAT2.
               EXIT.
          ENDIF.
     ENDIF.

      TL_OUT-LUDAT = TT_MSEG-BUDAT_MKPF.""最后耗用日期"
      TL_OUT-LUNUM = TT_MSEG-MENGE."最后耗用数量"
*    EXIT.
    ENDLOOP.
    IF TL_OUT-LUDAT IS INITIAL.
      "      READ TABLE TS_ZTMM_QCKL WITH KEY MATNR = TS_MARD-MATNR
      "                                    BWKEY = TS_MARD-WERKS
      "                                    LGORT = TS_MARD-LGORT.
      "      TL_OUT-LUDAT = TS_ZTMM_QCKL-BUDAT.
      "      TL_OUT-LUNUM = TS_ZTMM_QCKL-MENGE.
    ENDIF.

    TL_OUT-DNUM = TL_OUT-NUM5 + TL_OUT-NUM6 + TL_OUT-NUM7."呆滞数量  "TL_OUT-NUM4 +

*    IF TL_OUT-WERKS = '1100' AND TL_OUT-HPEINH NE 0.
*      TL_OUT-DSTRPS  = TL_OUT-DNUM * TL_OUT-HSTPRS / TL_OUT-HPEINH."呆滞金额
*    ELSEIF TL_OUT-PEINH NE 0.
      TL_OUT-DSTRPS  = TL_OUT-DNUM * TL_OUT-STPRS / TL_OUT-PEINH.
*    ENDIF.

    INDEX = INDEX + 1.
    TL_OUT-INDEX = INDEX.

    APPEND TL_OUT TO TS_OUT.
  ENDLOOP.
ENDFORM.                    " FRM_DEALDATA



FORM GET_PVPRS USING PYEAR TYPE PYEAR
                     PMONTH TYPE MONTH
                     PKALNR TYPE CKMLCR-KALNR
              CHANGING     PVPRS PEINH .
  P_YEAR = PYEAR.
  P_MONTH = PMONTH.
  CLEAR TS_CKMLCR.
  READ TABLE TS_CKMLCR WITH KEY BDATJ = PYEAR
                                POPER = PMONTH
                                KALNR = PKALNR.
  IF SY-SUBRC = 0.
    PVPRS = TS_CKMLCR-PVPRS.
    PEINH = TS_CKMLCR-PEINH.
  ELSE.
    IF P_MONTH = 1.
      P_MONTH = 12.
      P_YEAR = P_YEAR - 1.
    ELSE.
      P_MONTH = P_MONTH - 1.
    ENDIF.
    IF P_YEAR > MINYEAR.
      PERFORM GET_PVPRS USING P_YEAR P_MONTH PKALNR
                        CHANGING PVPRS PEINH.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV
*&---------------------------------------------------------------------*
*  功能描述：ALV显示子程序
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM FRM_ALV .

  PERFORM FRM_FIELDCAT.
  PERFORM FRM_LAYOUT.
  PERFORM FRM_OUTPUT.

ENDFORM.                    " FRM_ALV
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDCAT
*&---------------------------------------------------------------------*
*  功能描述：显示ALV列的标题、列宽、CHECKBOX
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM FRM_FIELDCAT .
  PERFORM FRM_DISP USING:
        'INDEX'  '序号'  '' '' '' '',
        'MTART'   '物料类型'  '' '' '' '',
        'MATKL'   '物料组'  '' '' '' '',
*        'EXTWG'     '外部物料组'  '' '' '' '',
        'MATNR'  '物料编号'  '' 'MATNR' 'MARA' 'X',
        'MAKTX'  '物料描述'  '' '' '' '',
        'MEINS'   '单位'  '' '' '' '',
        'WERKS'  '工厂'  '' '' '' '',
        'DISPO'  'MRP控制者'  '' '' '' '',
        'LGORT'  '库存地点'  '' '' '' '',
*        'LOGGR'  '仓管员代码'   '' '' '' '',
        'LABST'  '总数量'  '' '' '' '',
        'NDNUM'  '需求数量'  '' '' '' '',
        'STPRS'  '单价'  '' '' '' '',
        'PEINH'  '价格单位'  '' '' '' '',
*        'HSTPRS'  '单价(HKD)'  '' '' '' '',
*        'HPEINH'  '价格单位(HKD)'  '' '' '' '',
        'NUM1'   '1个月以内数量'  '' '' '' '',
        'STPRS1'  '1个月以内金额'  '' '' '' '',
        'NUM2'    '1-2月以内数量'  '' '' '' '',
        'STPRS2'  '1-2月以内金额'  '' '' '' '',
        'NUM3'   '2-3个月以内数量'  '' '' '' '',
        'STPRS3' '2-3个月以内金额'   '' '' '' '',
        'NUM4'   '3-6月以内数量'  '' '' '' '',
        'STPRS4' '3-6月以内金额'   '' '' '' '',
        'NUM5'   '6-12月以内数量'  '' '' '' '',
        'STPRS5' '6-12个月以内金额'   '' '' '' '',
        'NUM6'   '12-24个月以内数量'  '' '' '' '',
        'STPRS6'  '12-24个月以内金额'  '' '' '' '',
        'NUM7'   '大于两年以上数量'  '' '' '' '',
        'STPRS7' '大于两年以上金额'   '' '' '' '',
        'LTDAT'  '最后入库日期'  '' '' '' '',
        'LTNUM'  '最后入库数量'  '' '' '' '',
        'LUDAT'  '最后耗用日期'  '' '' '' '',
        'LUNUM'  '最后耗用数量'  '' '' '' '',
        'DNUM'  '呆滞数量'  '' '' '' '',
        'DSTRPS' '呆滞金额'  '' '' '' ''.

*价格权限控制
      AUTHORITY-CHECK OBJECT 'ZPRICE1' ID 'ZPRICE1' FIELD '1'.
      IF SY-SUBRC NE 0.
        DELETE IT_FIELDCAT WHERE SELTEXT_L CP '*金额' OR SELTEXT_L EQ '单价' OR SELTEXT_L EQ '价格单位'.
      ENDIF.
ENDFORM.                    " FRM_FIELDCAT
*&---------------------------------------------------------------------
*&      Form  FRM_FIELD_DISP
*&---------------------------------------------------------------------*
*  功能描述：通过传入字段名、字段名的相应描述，得到ALV视图的列标题
*----------------------------------------------------------------------*
*  入口参数：大写字段名、字段名的描述,列的宽度,参考的系统表字段名，参考
*            的系统表名（不需要的参数传为空）。
*  出口参数：无
*----------------------------------------------------------------------*
FORM FRM_DISP USING FU_NAME TYPE SLIS_FIELDCAT_ALV-FIELDNAME" 自定义数据库表字段
                    FU_DISP TYPE SLIS_FIELDCAT_ALV-SELTEXT_L" 字段描述
*                    fu_colwidth TYPE slis_fieldcat_alv-outputlen" 输出列的宽度
                    FU_NOZERO TYPE SLIS_FIELDCAT_ALV-NO_ZERO" 是否显示前导零
                    FU_REFNAME TYPE SLIS_FIELDCAT_ALV-REF_FIELDNAME" 参考的系统表字段
                    FU_REFTAB TYPE SLIS_FIELDCAT_ALV-REF_TABNAME" 参考的系统表
                    FU_JUST   TYPE SLIS_FIELDCAT_ALV-JUST.
  "形参类型与slis_fieldcat_alv中相应字段名的类型保持一致

  DATA: LW_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

  CLEAR LW_FIELDCAT.
  LW_FIELDCAT-FIELDNAME = FU_NAME.         " 自定义数据库表字段
  LW_FIELDCAT-REF_FIELDNAME = FU_REFNAME.  " 参考的系统表字段
  LW_FIELDCAT-REF_TABNAME = FU_REFTAB.     " 参考的系统表
  LW_FIELDCAT-SELTEXT_L = FU_DISP.         " 字段描述
*  lw_fieldcat-outputlen = fu_colwidth.     " 输出列的宽度
  LW_FIELDCAT-NO_ZERO = FU_NOZERO.         " 是否显示前导零
  LW_FIELDCAT-JUST = FU_JUST.
  APPEND LW_FIELDCAT TO IT_FIELDCAT.

ENDFORM.                    " FRM_DISP
*&---------------------------------------------------------------------*
*&      Form  FRM_LAYOUT
*&---------------------------------------------------------------------*
*  功能描述：显示ALV报表中的斑马线，设置ALV报表的第一列为CHECKBOX
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM FRM_LAYOUT .

*  wa_layout-colwidth_optimize = 'X'.     " ALV显示的列宽自动调整
  WA_LAYOUT-ZEBRA             = 'X'.     " ALV中显示斑马线
  WA_LAYOUT-BOX_FIELDNAME = 'CHECKBOX'.  " 第一列设置为CHECKBOX按钮
  WA_LAYOUT-COLTAB_FIELDNAME = 'CELLCOLOR'.

ENDFORM.                    " FRM_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  html_top_of_page
*&---------------------------------------------------------------------*
*  功能描述：采用html格式显示ALV表头标题
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM HTML_TOP_OF_PAGE USING P_CL_DD TYPE REF TO CL_DD_DOCUMENT  ##CALLED.

  DATA: M_P      TYPE I,
        M_BUFFER TYPE STRING,
        L_LINE   TYPE C LENGTH 1400,
        L_LENGTH TYPE I.

  M_BUFFER =
'<HTML><CENTER><H1><STRONG>库龄（呆滞料）查询报表</STRONG></H1></CENTER></HTML>'.
  CALL METHOD P_CL_DD->HTML_INSERT
    EXPORTING
      CONTENTS = M_BUFFER
    CHANGING
      POSITION = M_P.
  G_DAY = SY-DATUM+6(2).
  CONCATENATE '<P ALIGN = CENTER>'
              SY-DATUM+0(4) '年' SY-DATUM+4(2) '月' SY-DATUM+6(2) '日' INTO M_BUFFER.
  CALL METHOD P_CL_DD->HTML_INSERT
    EXPORTING
      CONTENTS = M_BUFFER
    CHANGING
      POSITION = M_P.


ENDFORM. "HTML_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*  功能描述：输出ALV报表的视图
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM FRM_OUTPUT .

  DATA: L_REPID    LIKE SY-REPID,
        GS_VARIANT TYPE DISVARIANT.

  GS_VARIANT-REPORT = SY-REPID.
  L_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PF_STATUS_SET    = 'FRM_SET_STATUS' "自定义按钮及事件
      I_CALLBACK_USER_COMMAND     = 'FRM_USER_COMMAND'
      I_CALLBACK_PROGRAM          = L_REPID
      IS_VARIANT                  = GS_VARIANT
      I_CALLBACK_HTML_TOP_OF_PAGE = 'HTML_TOP_OF_PAGE'
      I_HTML_HEIGHT_TOP           = 26
      IS_LAYOUT                   = WA_LAYOUT
      IT_FIELDCAT                 = IT_FIELDCAT[]
      I_SAVE                      = 'A'
    TABLES
      T_OUTTAB                    = TS_OUT
    EXCEPTIONS
      PROGRAM_ERROR               = 1
      OTHERS                      = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " FRM_OUTPUT


*&---------------------------------------------------------------------*
*&      Form  FRM_SET_STATUS
*&---------------------------------------------------------------------*
*  功能描述：在SE80中定义一个STANDARD_FULLSCREEN的状态栏
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM FRM_SET_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB..

  SET PF-STATUS 'STANDARD_FULLSCREEN'.    "定义GUI状态，添加应用工具栏按钮

ENDFORM.                    " FRM_SET_STATUS
*&---------------------------------------------------------------------*
*&      Form  FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*  功能描述：响应用户的操作
*----------------------------------------------------------------------*
*  入口参数：无
*  出口参数：无
*----------------------------------------------------------------------*
FORM FRM_USER_COMMAND USING UCOMM LIKE SY-UCOMM
                         SELFIELD TYPE SLIS_SELFIELD..


  CASE SY-UCOMM.    "sy-ucomm获得按钮的功能码

    WHEN 'BACK' OR 'EXIT'.
      LEAVE PROGRAM.

  ENDCASE.

ENDFORM.                    " FRM_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  FRM_NUMERIC_TO_CHAR
*&---------------------------------------------------------------------*
*  功能描述：将金额数字转换为文本输出，并添加千位分隔符
*----------------------------------------------------------------------*
*  入口参数：金额数字
*  出口参数：文本
*----------------------------------------------------------------------*
FORM FRM_NUMERIC_TO_CHAR USING FT_NUM  TYPE TSLXX12
                         CHANGING FC_CHAR TYPE CHAR31.

  DATA : LT_NUM TYPE TABLE OF STRING WITH HEADER LINE.
  DATA : L_NUM(50) TYPE C.         " 整数部分
  DATA : L_DEC TYPE STRING.         " 小数部分
  DATA : L_LEN TYPE I.
  DATA : L_SEP TYPE C VALUE '.' .

  DATA : L_TEMP_STR01 TYPE STRING.
  DATA : L_TEMP_STR02 TYPE C LENGTH 1.
  DATA : L_TEMP_STR03 TYPE STRING.

  MOVE FT_NUM TO FC_CHAR.

* 在字符串里加逗号
  SPLIT FC_CHAR AT L_SEP INTO TABLE LT_NUM IN CHARACTER MODE.

* 获取整数部分和小数部分
  LOOP AT LT_NUM.
    L_TEMP_STR01 = LT_NUM.
    AT FIRST.
      L_NUM = L_TEMP_STR01.
    ENDAT.
    AT LAST.
      L_DEC = L_TEMP_STR01.
    ENDAT.
  ENDLOOP.

  L_LEN = STRLEN( L_NUM ).

  DO L_LEN TIMES.
    L_LEN = L_LEN - 1.
    L_TEMP_STR02 = L_NUM+L_LEN(1).
    CONCATENATE L_TEMP_STR03 L_TEMP_STR02 INTO L_TEMP_STR03.
  ENDDO.

  L_NUM = L_TEMP_STR03.

  CLEAR : LT_NUM.
  REFRESH : LT_NUM.

  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      TEXTLINE            = L_NUM
      DELIMITER           = ''
      OUTPUTLEN           = 3
* IMPORTING
*     OUT_LINE1           =
*     OUT_LINE2           =
*     OUT_LINE3           =
    TABLES
      OUT_LINES           = LT_NUM
    EXCEPTIONS
      OUTPUTLEN_TOO_LARGE = 1
      OTHERS              = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR : L_NUM.

  LOOP AT LT_NUM.
    CONCATENATE L_NUM ',' LT_NUM INTO L_NUM.
  ENDLOOP.

  SHIFT L_NUM BY 1 PLACES LEFT.

  L_LEN = STRLEN( L_NUM ).
  CLEAR : L_TEMP_STR03.

  DO L_LEN TIMES.
    L_LEN = L_LEN - 1.
    L_TEMP_STR02 = L_NUM+L_LEN(1).
    CONCATENATE L_TEMP_STR03 L_TEMP_STR02 INTO L_TEMP_STR03.
  ENDDO.

  L_NUM = L_TEMP_STR03.

* 重新赋值
  CLEAR : FC_CHAR.
  CONCATENATE L_NUM '.' L_DEC INTO FC_CHAR.

  SHIFT FC_CHAR RIGHT DELETING TRAILING SPACE.

ENDFORM.                    " FRM_NUMERIC_TO_CHAR
" FRM_CELLCOLOR
