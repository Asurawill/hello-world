*&---------------------------------------------------------------------*
*& 程序名称:ZMM046
*& 作者    :余柏烨
*& 开发日期:
*& 请求号  :
*& 描述    :物料主数据接口
*& 开发申请：
*& 变更记录
*&
** 修改日期      开发人员     请求号           描述
*&---------------------------------------------------------------------*

REPORT ZMM046_DROP.

*--------------------------------------------------------------------*
*------- DBCO 里的连接信息   -------------------------------*
*--MSSQL_SERVER=10.1.212.250 MSSQL_DBNAME=leyard OBJECT_SOURCE=ZMMT001   ------------*
*----连接名称： Z_CONN   ----------------------------------------*
*-----------------------------------------------------------------*

" 连接的名称
DATA G_CONEXION LIKE DBCON-CON_NAME VALUE 'Z_CONN'.

" 数据发送返回错误消息
DATA: EXEC_REF   TYPE REF TO CX_SY_NATIVE_SQL_ERROR,
      ERROR_TEXT TYPE STRING.

DATA: G_ERRORSTR(250) TYPE C.

DATA: BEGIN OF WA_TRANS ,
        CLIENT    TYPE SY-MANDT,
        MATNR     TYPE MARA-MATNR,
        LVORM     TYPE MARA-LVORM,
        MAKTX     TYPE MAKT-MAKTX,
        EXTWG     TYPE MARA-EXTWG,
        ATWRT     TYPE AUSP-ATWRT,
        STPRS     TYPE MBEW-STPRS,
        LABST     TYPE MARD-LABST,
        MEINS     TYPE MARA-MEINS,
        PEINH     TYPE MBEW-PEINH,
        MATKL     TYPE MARA-MATKL,
        BISMT     TYPE MARA-BISMT,
        ERSDA(10) TYPE C,
        LAEDA(10) TYPE C,
      END OF WA_TRANS.

DATA: DBCUR TYPE CURSOR.

START-OF-SELECTION.

*首先建立数据库连接
  PERFORM SUB_CONNDB USING G_CONEXION.
*  PERFORM GETDATA.
  "PERFORM updatedata.
  PERFORM INSERTDATA. " 传输数据

END-OF-SELECTION.

FORM SUB_CONNDB USING CONN LIKE DBCON-CON_NAME.
  CLEAR G_ERRORSTR.
  G_CONEXION = CONN.
  TRY.
      "--  连接SQL SERVER
      EXEC SQL.
        CONNECT TO :G_CONEXION
      ENDEXEC.
      EXEC SQL.
        SET CONNECTION :G_CONEXION
      ENDEXEC.
    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXEC_REF.
      CONCATENATE '无法连接至SQL数据库   连接名:' G_CONEXION INTO G_ERRORSTR.

  ENDTRY.
ENDFORM.
"-- 获取 SQL SERVER 里的表数据 方法一
*FORM GETDATA.
*
**-- 表  TABLE_2 是SQL 数据库 saptest  里存在的表
*
*  TRY.
*      EXEC SQL.
*        OPEN dbcur FOR  SELECT *  FROM  ZMMT001
*      ENDEXEC.
*      WRITE:/ 'CLIENT' ,  'NAME' ,  'TEXT'.
*
*      DO.
*        EXEC SQL.
*          FETCH NEXT dbcur INTO :wa_trans.
*        ENDEXEC.
*        IF SY-SUBRC <> 0.
*          EXIT.
*        ELSE.
*          WRITE:/ WA_TRANS-CLIENT ,  WA_TRANS-NAME ,  WA_TRANS-TEXT.
*        ENDIF.
*      ENDDO.
*
*      EXEC SQL.
*        CLOSE dbcur
*      ENDEXEC.
*
*    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXEC_REF.
*      G_ERRORSTR = EXEC_REF->GET_TEXT( ).
*      CONCATENATE '读取数据出错,'  G_ERRORSTR INTO G_ERRORSTR.
*
*      WRITE:/ G_ERRORSTR.
*  ENDTRY.
*
*ENDFORM.
"-- 获取 SQL SERVER 里的表数据 方法二 ： 不使用游标
FORM GETDATA2.

  TRY.
      EXEC SQL PERFORMING LOOP_OUTPUT.
        SELECT *
          INTO :WA_TRANS
          FROM ZMMT001
      ENDEXEC.

    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXEC_REF.
      G_ERRORSTR = EXEC_REF->GET_TEXT( ).
      CONCATENATE '读取数据出错,'  G_ERRORSTR INTO G_ERRORSTR.

      WRITE:/ G_ERRORSTR.
  ENDTRY.

ENDFORM.
FORM LOOP_OUTPUT.
  WRITE :/ 'WA_TRANS-CLIENT',' ','WA_TRANS-MATNR',' ','WA_TRANS-MAKTX',' ',
            'WA_TRANS-EXTWG',' ','WA_TRANS-ATWRT',' ','WA_TRANS-STPRS',' ',
            'WA_TRANS-LABST',' ','WA_TRANS-MEINS',' ','WA_TRANS-PEINH',' ',
            'WA_TRANS-MATKL',' ','WA_TRANS-BISMT',' ','WA_TRANS-ERSDA',' ',
            'WA_TRANS-LAEDA',' ','WA_TRANS-LVORM' .
  WRITE :/ WA_TRANS-CLIENT,' ',WA_TRANS-MATNR,' ',WA_TRANS-MAKTX,' ',
            WA_TRANS-EXTWG,' ',WA_TRANS-ATWRT,' ',WA_TRANS-STPRS,' ',
            WA_TRANS-LABST,' ',WA_TRANS-MEINS,' ',WA_TRANS-PEINH,' ',
            WA_TRANS-MATKL,' ',WA_TRANS-BISMT,' ',WA_TRANS-ERSDA,' ',
            WA_TRANS-LAEDA,' ',WA_TRANS-LVORM .
ENDFORM.
FORM UPDATEDATA.

  TRY.
      EXEC SQL .
        update TABLE_2 set list = 11 where name = 1
      ENDEXEC.

    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXEC_REF.
      G_ERRORSTR = EXEC_REF->GET_TEXT( ).
      CONCATENATE '更新数据出错,'  G_ERRORSTR INTO G_ERRORSTR.

      WRITE:/ G_ERRORSTR.
  ENDTRY.

  PERFORM GETDATA2.

ENDFORM.
FORM INSERTDATA.

*  DATA: BEGIN OF LS_MARA ,
*          MATNR TYPE MARA-MATNR,
*          EXTWG TYPE MARA-EXTWG,
*          MEINS TYPE MARA-MEINS,
*          MATKL TYPE MARA-MATKL,
*          BISMT TYPE MARA-BISMT,
*          ERSDA TYPE MARA-ERSDA,
*          LAEDA TYPE MARA-LAEDA,
*          LVORM TYPE MARA-LVORM,
*        END OF LS_MARA .
*  DATA LT_MARA LIKE TABLE OF LS_MARA .
*  DATA: BEGIN OF LS_OBJEK ,
*          OBJEK TYPE AUSP-OBJEK,
*        END OF LS_OBJEK .
*  DATA LT_OBJEK LIKE TABLE OF LS_OBJEK .
*  DATA: BEGIN OF LS_MAKT ,
*          MATNR TYPE MAKT-MATNR,
*          MAKTX TYPE MAKT-MAKTX,
*        END OF LS_MAKT .
*  DATA LT_MAKT LIKE TABLE OF LS_MAKT .
*  DATA: BEGIN OF LS_AUSP ,
*          OBJEK TYPE AUSP-OBJEK,
*          ATINN TYPE AUSP-ATINN,
*          ATWRT TYPE AUSP-ATWRT,
*        END OF LS_AUSP .
*  DATA LT_AUSP LIKE TABLE OF LS_AUSP .
*  DATA: BEGIN OF LS_MBEW ,
*          MATNR TYPE MBEW-MATNR,
*          BWKEY TYPE MBEW-BWKEY,
*          VPRSV TYPE MBEW-VPRSV,
*          STPRS TYPE MBEW-STPRS,
*          ZPLP2 TYPE MBEW-ZPLP2,
*          PEINH TYPE MBEW-PEINH,
*        END OF LS_MBEW .
*  DATA LT_MBEW LIKE TABLE OF LS_MBEW .
*  DATA: BEGIN OF LS_EKPO ,
*          EBELN TYPE EKPO-EBELN,
*          EBELP TYPE EKPO-EBELP,
*          MATNR TYPE EKPO-MATNR,
*          AEDAT TYPE EKPO-AEDAT,
*          KBETR TYPE KONV-KBETR,
*          KPEIN TYPE KONV-KPEIN,
*        END OF LS_EKPO .
*  DATA LT_EKPO LIKE TABLE OF LS_EKPO .
*  DATA: BEGIN OF LS_MARD ,
*          MATNR TYPE MARD-MATNR,
*          WERKS TYPE MARD-WERKS,
*          LGORT TYPE MARD-LGORT,
*          LABST TYPE MARD-LABST,
*        END OF LS_MARD .
*  DATA LT_MARD LIKE TABLE OF LS_MARD .
*
*  " 要传输的数据结构
*  DATA: BEGIN OF LS_ZMMT001 ,
*          CLIENT    TYPE SY-MANDT,
*          MATNR     TYPE MARA-MATNR,
*          LVORM     TYPE MARA-LVORM,
*          MAKTX     TYPE MAKT-MAKTX,
*          EXTWG     TYPE MARA-EXTWG,
*          ATWRT     TYPE AUSP-ATWRT,
*          STPRS     TYPE MBEW-STPRS,
*          LABST     TYPE MARD-LABST,
*          MEINS     TYPE MARA-MEINS,
*          PEINH     TYPE MBEW-PEINH,
*          MATKL     TYPE MARA-MATKL,
*          BISMT     TYPE MARA-BISMT,
*          ERSDA(10) TYPE C,
*          LAEDA(10) TYPE C,
*        END OF LS_ZMMT001 .
*  DATA LT_ZMMT001 LIKE TABLE OF LS_ZMMT001 .
*
** 增量表
*  DATA: LT_ZMMT001X TYPE TABLE OF ZMMT001,
*        LS_ZMMT001X TYPE ZMMT001.
*  DATA L_DATE_NOW TYPE P0001-BEGDA .
*  DATA L_DATE TYPE P0001-BEGDA .
*
**  SELECT *
**    INTO TABLE LT_ZMMT001X
**    FROM ZMMT001 .
**  SORT LT_ZMMT001X BY ZXH DESCENDING .
**  READ TABLE LT_ZMMT001X INTO LS_ZMMT001X INDEX 1 .
*
*  " 取当前日期 前5天的数据
*  L_DATE_NOW = SY-DATUM .
*  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*    EXPORTING
*      DATE      = L_DATE_NOW
*      DAYS      = 05
*      MONTHS    = 00
*      SIGNUM    = '-'
*      YEARS     = 00
*    IMPORTING
*      CALC_DATE = L_DATE.
*
*  SELECT A~MATNR
*         A~EXTWG
*         A~MEINS
*         A~MATKL
*         A~BISMT
*         A~ERSDA
*         A~LAEDA
*         A~LVORM
*    INTO CORRESPONDING FIELDS OF TABLE LT_MARA
*    FROM MARA AS A
*   INNER JOIN MARC AS B
*      ON A~MATNR = B~MATNR
*   WHERE B~WERKS = '1700'
*     AND ( A~ERSDA GE L_DATE
*      OR   A~LAEDA GE L_DATE ) .
*  IF LT_MARA IS NOT INITIAL .
*
*    LOOP AT LT_MARA INTO LS_MARA .
*      LS_OBJEK-OBJEK = LS_MARA-MATNR .
*      APPEND LS_OBJEK TO LT_OBJEK .
*      CLEAR LS_OBJEK .
*      CLEAR LS_MARA .
*    ENDLOOP.
*
*    SELECT MATNR
*           MAKTX
*      INTO CORRESPONDING FIELDS OF TABLE LT_MAKT
*      FROM MAKT
*       FOR ALL ENTRIES IN LT_MARA
*     WHERE MATNR = LT_MARA-MATNR
*       AND SPRAS = SY-LANGU .
*    SORT LT_MAKT BY MATNR .
*
*    SELECT OBJEK
*           ATINN
*           ATWRT
*      INTO CORRESPONDING FIELDS OF TABLE LT_AUSP
*      FROM AUSP
*       FOR ALL ENTRIES IN LT_OBJEK
*     WHERE OBJEK = LT_OBJEK-OBJEK
*       AND ATINN = 'GGXH' .
*    SORT LT_AUSP BY OBJEK .
*
*    SELECT MATNR
*           BWKEY
*           VPRSV
*           STPRS
*           ZPLP2
*           PEINH
*      INTO CORRESPONDING FIELDS OF TABLE LT_MBEW
*      FROM MBEW
*       FOR ALL ENTRIES IN LT_MARA
*     WHERE MATNR = LT_MARA-MATNR
*       AND BWKEY = '1700' .
*    SORT LT_MBEW BY MATNR .
*
*    SELECT A~EBELN
*           A~EBELP
*           A~MATNR
*           A~AEDAT
*           C~KBETR
*           C~KPEIN
*      INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
*      FROM EKPO AS A
*     INNER JOIN EKKO AS B
*        ON A~EBELN = B~EBELN
*     INNER JOIN KONV AS C
*        ON B~KNUMV = C~KNUMV
*       FOR ALL ENTRIES IN LT_MARA
*     WHERE A~MATNR = LT_MARA-MATNR
*       AND C~KSCHL = 'PBXX' .
*    SORT LT_EKPO BY MATNR ASCENDING AEDAT DESCENDING .
*    DELETE ADJACENT DUPLICATES FROM LT_EKPO COMPARING MATNR .
*
*    SELECT MATNR
*           WERKS
*           LGORT
*           LABST
*      INTO CORRESPONDING FIELDS OF TABLE LT_MARD
*      FROM MARD
*       FOR ALL ENTRIES IN LT_MARA
*     WHERE MATNR = LT_MARA-MATNR
*       AND WERKS = '1700' .
*    SORT LT_MARD BY MATNR .
*
** 将数据放进内保
*    LOOP AT LT_MARA INTO LS_MARA .
*      LS_ZMMT001-CLIENT = SY-MANDT .
*      LS_ZMMT001-MATNR = LS_MARA-MATNR .
*      LS_ZMMT001-LVORM = LS_MARA-LVORM .
*      LS_ZMMT001-EXTWG = LS_MARA-EXTWG .
*      LS_ZMMT001-MEINS = LS_MARA-MEINS .
*      LS_ZMMT001-MATKL = LS_MARA-MATKL .
*      LS_ZMMT001-BISMT = LS_MARA-BISMT .
*      " 日期格式转换
*      CONCATENATE LS_MARA-ERSDA+0(4) '-' LS_MARA-ERSDA+4(2) '-' LS_MARA-ERSDA+6(2) INTO LS_ZMMT001-ERSDA .
**      LS_ZMMT001-ERSDA = LS_MARA-ERSDA .
*      CONCATENATE LS_MARA-LAEDA+0(4) '-' LS_MARA-LAEDA+4(2) '-' LS_MARA-LAEDA+6(2) INTO LS_ZMMT001-LAEDA .
**      LS_ZMMT001-LAEDA = LS_MARA-LAEDA .
*
*      READ TABLE LT_MAKT INTO LS_MAKT WITH KEY MATNR = LS_MARA-MATNR BINARY SEARCH .
*      LS_ZMMT001-MAKTX = LS_MAKT-MAKTX .
*
*      READ TABLE LT_AUSP INTO LS_AUSP WITH KEY OBJEK = LS_MARA-MATNR BINARY SEARCH .
*      LS_ZMMT001-ATWRT = LS_AUSP-ATWRT .
*
*      READ TABLE LT_MBEW INTO LS_MBEW WITH KEY MATNR = LS_MARA-MATNR BINARY SEARCH .
*      IF LS_MBEW-VPRSV = 'V'.
*        READ TABLE LT_EKPO INTO LS_EKPO WITH KEY MATNR = LS_MARA-MATNR BINARY SEARCH .
*        LS_ZMMT001-STPRS = LS_EKPO-KBETR .
*        LS_ZMMT001-PEINH = LS_EKPO-KPEIN .
*      ELSEIF LS_MBEW-VPRSV = 'S'.
*        IF LS_MBEW-STPRS NE 0 AND LS_MBEW-STPRS IS NOT INITIAL .
*          LS_ZMMT001-STPRS = LS_MBEW-STPRS .
*        ELSE .
*          LS_ZMMT001-STPRS = LS_MBEW-ZPLP2 .
*        ENDIF.
*        LS_ZMMT001-PEINH = LS_MBEW-PEINH .
*      ENDIF.
*
*      READ TABLE LT_MARD WITH KEY MATNR = LS_MARA-MATNR BINARY SEARCH TRANSPORTING NO FIELDS .
*      IF SY-SUBRC = 0 .
*        LOOP AT LT_MARD INTO LS_MARD FROM SY-TABIX .
*          IF LS_MARD-MATNR = LS_MARA-MATNR .
*            LS_ZMMT001-LABST = LS_ZMMT001-LABST + LS_MARD-LABST .
*            CLEAR LS_MARD .
*          ELSE .
*            CLEAR LS_MARD .
*            EXIT .
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*
*      APPEND LS_ZMMT001 TO LT_ZMMT001 .
*      CLEAR LS_ZMMT001 .
*
*      CLEAR LS_MARD .
*      CLEAR LS_AUSP .
*      CLEAR LS_EKPO .
*      CLEAR LS_AUSP .
*      CLEAR LS_MAKT .
*      CLEAR LS_MARA .
*    ENDLOOP.
*
*  ENDIF.

* 将数据写入到数据库
*  LOOP AT LT_ZMMT001 INTO LS_ZMMT001 .
  TRY.

*      EXEC SQL .
*        CREATE TABLE ZMMT001(
*          CLIENT VARCHAR(3) NOT NULL ,
*          MATNR VARCHAR(30) NOT NULL ,
*          LVORM VARCHAR(1) ,
*          MAKTX VARCHAR(100) ,
*          EXTWG VARCHAR(30) ,
*          ATWRT VARCHAR(60) ,
*          STPRS DECIMAL(11,2) ,
*          LABST DECIMAL(13,3) ,
*          MEINS VARCHAR(10) ,
*          PEINH INT ,
*          MATKL VARCHAR(20) ,
*          BISMT VARCHAR(30) ,
*          ERSDA VARCHAR(20) ,
*          LAEDA VARCHAR(20) ,
*          PRIMARY KEY (CLIENT,MATNR)
*        )
*      ENDEXEC.

*      EXEC SQL .
*        insert ZMMT001(CLIENT,NAME,TEXT) values(800,'A003','ABCDEFG3')
*      ENDEXEC .

      EXEC SQL .
        DROP TABLE ZMMT001
      ENDEXEC .

*        EXEC SQL .
*          INSERT INTO ZMMT001 VALUES(:LS_ZMMT001-CLIENT,
*                                     :LS_ZMMT001-MATNR,
*                                     :LS_ZMMT001-MAKTX,
*                                     :LS_ZMMT001-EXTWG,
*                                     :LS_ZMMT001-ATWRT,
*                                     :LS_ZMMT001-STPRS,
*                                     :LS_ZMMT001-LABST,
*                                     :LS_ZMMT001-MEINS,
*                                     :LS_ZMMT001-PEINH,
*                                     :LS_ZMMT001-MATKL,
*                                     :LS_ZMMT001-BISMT,
*                                     :LS_ZMMT001-ERSDA,
*                                     :LS_ZMMT001-LAEDA)
*        ENDEXEC .

*      EXEC SQL .
*
*        INSERT INTO ZMMT001(LT_ZMMT001)
*
*      ENDEXEC .

    CATCH CX_SY_NATIVE_SQL_ERROR INTO EXEC_REF.
      G_ERRORSTR = EXEC_REF->GET_TEXT( ).
      CONCATENATE '插入数据出错,'  G_ERRORSTR INTO G_ERRORSTR.

      WRITE:/ G_ERRORSTR.
  ENDTRY.

*    CLEAR LS_ZMMT001 .
*  ENDLOOP.

*  IF G_ERRORSTR IS INITIAL .
*    CLEAR LS_ZMMT001X .
*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        NR_RANGE_NR             = '00'
*        OBJECT                  = 'ZMMT001'
*      IMPORTING
*        NUMBER                  = LS_ZMMT001X-ZXH
*      EXCEPTIONS
*        INTERVAL_NOT_FOUND      = 1
*        NUMBER_RANGE_NOT_INTERN = 2
*        OBJECT_NOT_FOUND        = 3
*        QUANTITY_IS_0           = 4
*        QUANTITY_IS_NOT_1       = 5
*        INTERVAL_OVERFLOW       = 6
*        BUFFER_OVERFLOW         = 7
*        OTHERS                  = 8.
*    LS_ZMMT001X-ZDATE = SY-DATUM .
*    LS_ZMMT001X-ZTIME = SY-UZEIT .
*    INSERT ZMMT001 FROM LS_ZMMT001X .
*    IF SY-SUBRC = 0 .
*      COMMIT WORK .
*    ELSE .
*      ROLLBACK WORK .
*    ENDIF.
*  ENDIF.

*  REFRESH LT_ZMMT001X .

  PERFORM GETDATA2.

ENDFORM.
