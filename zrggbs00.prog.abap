PROGRAM zrggbs00 .
*---------------------------------------------------------------------*
* Corrections/ repair
* wms092357 070703 Note 638886: template routines to be used for
*                  workaround to substitute bseg-bewar from bseg-xref1/2
*---------------------------------------------------------------------*
*                                                                     *
*   Substitutions: EXIT-Formpool for Uxxx-Exits                       *
*                                                                     *
*   This formpool is used by SAP for testing purposes only.           *
*                                                                     *
*   Note: If you define a new user exit, you have to enter your       *
*         user exit in the form routine GET_EXIT_TITLES.              *
*                                                                     *
*---------------------------------------------------------------------*
INCLUDE fgbbgd00.              "Standard data types


*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
*    PLEASE INCLUDE THE FOLLOWING "TYPE-POOL"  AND "TABLES" COMMANDS  *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM         *
TYPE-POOLS: gb002. " TO BE INCLUDED IN                       "wms092357
TABLES: bkpf,      " ANY SYSTEM THAT                         "wms092357
        bseg,      " HAS 'FI' INSTALLED                      "wms092357
        cobl,                                               "wms092357
        csks,                                               "wms092357
        anlz,                                               "wms092357
        glu1.                                               "wms092357
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*


*----------------------------------------------------------------------*
*       FORM GET_EXIT_TITLES                                           *
*----------------------------------------------------------------------*
*       returns name and title of all available standard-exits         *
*       every exit in this formpool has to be added to this form.      *
*       You have to specify a parameter type in order to enable the    *
*       code generation program to determine correctly how to          *
*       generate the user exit call, i.e. how many and what kind of    *
*       parameter(s) are used in the user exit.                        *
*       The following parameter types exist:                           *
*                                                                      *
*       TYPE                Description              Usage             *
*    ------------------------------------------------------------      *
*       C_EXIT_PARAM_NONE   Use no parameter         Subst. and Valid. *
*                           except B_RESULT                            *
*       C_EXIT_PARAM_FIELD  Use one field as param.  Only Substitution *
*       C_EXIT_PARAM_CLASS  Use a type as parameter  Subst. and Valid  *
*                                                                      *
*----------------------------------------------------------------------*
*  -->  EXIT_TAB  table with exit-name and exit-titles                 *
*                 structure: NAME(5), PARAM(1), TITEL(60)
*----------------------------------------------------------------------*
FORM get_exit_titles TABLES etab.

  DATA: BEGIN OF exits OCCURS 50,
          name(5)   TYPE c,
          param     LIKE c_exit_param_none,
          title(60) TYPE c,
        END OF exits.

  exits-name  = 'U100'.
  exits-param = c_exit_param_none.
  exits-title = text-100.             "Cost center from CSKS
  APPEND exits.

  exits-name  = 'U101'.
  exits-param = c_exit_param_field.
  exits-title = text-101.             "Cost center from CSKS
  APPEND exits.

* begin of insertion                                          "wms092357
  exits-name  = 'U200'.
  exits-param = c_exit_param_field.
  exits-title = text-200.             "Cons. transaction type
  APPEND exits.                       "from xref1/2
* end of insertion                                            "wms092357

  exits-name  = 'U102'.
  exits-param = c_exit_param_none.
  exits-title = text-100.             "Cost center from CSKS
  APPEND exits.
*  应付暂估规则统一文本及分配
  exits-name  = 'U300'.
  exits-param = c_exit_param_none.
  exits-title = text-300.             "Cost center from CSKS
  APPEND exits.

************************************************************************
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  EXITS-NAME  = 'U102'.
*  EXITS-PARAM = C_EXIT_PARAM_CLASS.
*  EXITS-TITLE = TEXT-102.             "Sum is used for the reference.
*  APPEND EXITS.


***********************************************************************
** EXIT EXAMPLES FROM PUBLIC SECTOR INDUSTRY SOLUTION
**
** PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINE
** TO ENABLE PUBLIC SECTOR EXAMPLE SUBSTITUTION EXITS
***********************************************************************
  INCLUDE rggbs_ps_titles.

  REFRESH etab.
  LOOP AT exits.
    etab = exits.
    APPEND etab.
  ENDLOOP.

ENDFORM.                    "GET_EXIT_TITLES


* eject
*---------------------------------------------------------------------*
*       FORM U100                                                     *
*---------------------------------------------------------------------*
*       Reads the cost-center from the CSKS table .                   *
*---------------------------------------------------------------------*
FORM u100.
  DATA: l_field(50).
  DATA l_flag TYPE c. "接收内存值：当前是否存在z01 z02 项目类别
  DATA lw_vbak TYPE vbak.
  DATA lw_vbap TYPE vbap.
  DATA:lt_vbap TYPE TABLE OF vbap.
  RANGES l_hkont FOR bseg-hkont.
  RANGES l_hkont2 FOR bseg-hkont.
  RANGES l_hkont3 FOR bseg-hkont. "it02 150602 add
  RANGES l_hkont4 FOR bseg-hkont. "it02 150602 add
  RANGES l_hkont5 FOR bseg-hkont. "it02 150602 add
  CLEAR: l_hkont[],l_hkont2[].
  "MIRO,MIR4,MIR7,1000公司代码，如果物料组为Z00028~Z00037且科目为2202010101则替换为220201201 BY HANDTB 20150330


  DATA l_matkl TYPE mara-matkl.
  DATA l_varname TYPE char20.
  FIELD-SYMBOLS <lfs_matkl> TYPE matkl.
  l_varname = '()YDRSEG-MATKL'.
  ASSIGN (l_varname) TO <lfs_matkl>.
  IF sy-tcode EQ 'MIRO'
    OR sy-tcode EQ 'MIR4'
    OR sy-tcode EQ 'MR8M'
    OR sy-tcode EQ 'MIR7'.
    IF bseg-bukrs EQ '1000' AND bseg-hkont EQ '2202010101'.
      IF <lfs_matkl> GE 'Z00028'
        AND <lfs_matkl> LE 'Z00040'.                                    " AND <lfs_matkl> LE 'Z00037'. "2202010101 :应付账款-非关联方-采购  2202010201：应付账款-非关联方-分包
        bseg-hkont = '2202010201'.
      ENDIF.
    ENDIF.
  ENDIF.

*ADD BY  HANDWY 2015-7-29 功能范围替代
  DATA gs_prps TYPE prps.
  DATA gs_csks TYPE csks.
  CLEAR gs_prps.
  CLEAR gs_csks.


*&--代码注释 BY HANDYBY 22.05.2017 17:30:21  BEGIN
*  IF BSEG-BUKRS  = '1800'
*&--代码注释 BY HANDYBY 22.05.2017 17:30:21  END
*&--代码添加 BY HANDYBY 22.05.2017 17:30:31  BEGIN
  IF ( bseg-bukrs  = '1800' OR bseg-bukrs  = '1700' OR bseg-bukrs  = '1710' OR
       bseg-bukrs  = '2000' OR bseg-bukrs  = '2300' OR bseg-bukrs  = '2310' OR
       bseg-bukrs  = '2320' OR bseg-bukrs  = '2700' OR bseg-bukrs  = '2800' OR
       bseg-bukrs  = '3000' )
*&--代码添加 BY HANDYBY 22.05.2017 17:30:31  END

  AND bseg-hkont+0(1) = '8'.

    SELECT SINGLE * FROM prps
     INTO CORRESPONDING FIELDS OF gs_prps
     WHERE pspnr = bseg-projk.

*&--代码注释 BY HANDYBY 22.05.2017 17:34:40  BEGIN
*   IF GS_PRPS-PRART = '20'.
*&--代码注释 BY HANDYBY 22.05.2017 17:34:40  END
*&--代码添加 BY HANDYBY 22.05.2017 17:34:47  BEGIN
    IF gs_prps-prart = '20' OR gs_prps-prart = '51' OR gs_prps-prart = '56' OR
        gs_prps-prart = '62' OR gs_prps-prart = '72' OR gs_prps-prart = '81' OR
      gs_prps-prart = '82' OR gs_prps-prart = '86' .
*&--代码添加 BY HANDYBY 22.05.2017 17:34:47  END

      SELECT SINGLE * FROM csks
        INTO CORRESPONDING FIELDS OF gs_csks
        WHERE kostl = bseg-kostl.
      IF sy-subrc = 0.
        bseg-fkber = gs_csks-func_area.
      ENDIF.
    ENDIF.
  ENDIF.
*ENDADD.

*ADD BY HANDWY  2015-7-29 科目替代

*&--代码注释 BY HANDYBY 22.05.2017 17:36:54  BEGIN
*    IF BSEG-BUKRS = '1800'
*&--代码注释 BY HANDYBY 22.05.2017 17:36:54  END
*&--代码添加 BY HANDYBY 22.05.2017 17:37:02  BEGIN
  IF ( bseg-bukrs  = '1800' OR bseg-bukrs  = '1700' OR bseg-bukrs  = '1710' OR
       bseg-bukrs  = '2000' OR bseg-bukrs  = '2300' OR bseg-bukrs  = '2310' OR
       bseg-bukrs  = '2320' OR bseg-bukrs  = '2700' OR bseg-bukrs  = '2800' OR
       bseg-bukrs  = '3000' )
*&--代码添加 BY HANDYBY 22.05.2017 17:37:02  END

AND bseg-hkont = '5401010101'.
    CLEAR gs_prps.

    SELECT SINGLE  * FROM  prps
      INTO CORRESPONDING FIELDS OF gs_prps
      WHERE pspnr = bseg-projk.

*&--代码注释 BY HANDYBY 22.05.2017 17:38:23  BEGIN
*    IF GS_PRPS-PRART = '10'
*    OR GS_PRPS-PRART = '40'.
*&--代码注释 BY HANDYBY 22.05.2017 17:38:23  END
*&--代码添加 BY HANDYBY 22.05.2017 17:38:32  BEGIN
    IF gs_prps-prart = '10'
    OR gs_prps-prart = '40' OR gs_prps-prart = '55' OR gs_prps-prart = '61'
      OR gs_prps-prart = '80' .
*&--代码添加 BY HANDYBY 22.05.2017 17:38:32  END

      bseg-hkont = '8005000001'.
    ENDIF.
  ENDIF.
*ENDADD.

*ADD BY HANDWY 2015-7-17  VF01开票抬头文本带入会计凭证抬头行项目
  IF sy-tcode = 'VF01'.
    DATA: lv_name TYPE thead-tdname,
          lt_line TYPE STANDARD TABLE OF tline,
          lw_line TYPE tline.

    lv_name = bkpf-belnr.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = '0002'
        language                = sy-langu
        name                    = lv_name
        object                  = 'VBBK'
      TABLES
        lines                   = lt_line[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc EQ 0.
      READ TABLE lt_line
        INTO lw_line
        INDEX 1.
*      IN BILLING HEADER TEXT, THE SIGN '&' (AMPERSAND) IS AUTOMATICALLY
*      ADDED SOME CHARACTERS SO IT BECOMES '<(>&<)>'.
*      THE LOGIC BELOW IS TO FIX THAT ISSUE.
      REPLACE ALL OCCURRENCES OF '<(>' IN lw_line-tdline WITH space.
      REPLACE ALL OCCURRENCES OF '<)>' IN lw_line-tdline WITH space.
      bseg-sgtxt = lw_line-tdline.
      bkpf-bktxt = lw_line-tdline.
    ENDIF.
  ENDIF.
*EDD.

*  "免费项目发货科目替代   2015.4.16去除
*  IF   sy-tcode = 'VL01N'
*    OR sy-tcode = 'VL02N'
*    OR sy-tcode = 'VL06G'
*    OR sy-tcode = 'VL09'.
*
*    DATA l_vbak TYPE vbak.
*    SELECT SINGLE vbeln auart INTO CORRESPONDING FIELDS OF l_vbak
*      FROM vbak
*      WHERE vbeln = bseg-vbel2.
*    IF sy-subrc = 0 AND l_vbak-auart = 'ZF1'.
*      l_hkont-sign = 'I'.
*      l_hkont-option = 'BT'.
*      l_hkont-low  = '6404000000'.
*      l_hkont-high = '6404999999'.
*      APPEND l_hkont.
*      IF bseg-hkont IN l_hkont.
*        bseg-hkont = '6401010101'.
*      ENDIF.
*      CLEAR l_hkont[].
*    ENDIF.
*  ENDIF.
  DATA:posnrtmp TYPE vbap-posnr .
  CLEAR l_flag.
  IMPORT l_flag FROM MEMORY ID 'PSTYV_FLAG'."读取内存
  IF l_flag = ''.
    SELECT SINGLE COUNT(*) FROM vbap
      WHERE vbeln = bkpf-xblnr
      AND   pstyv IN ('Z01','Z02')
      .
    IF sy-subrc = 0.
      l_flag = 'X'.
    ENDIF.
  ENDIF.

  IF l_flag = 'X'.

    l_hkont-sign = 'I'.
    l_hkont-option = 'BT'.
    l_hkont-low = '2221030501'.
    l_hkont-high = '2221030502'.
    APPEND l_hkont.
    l_hkont-sign = 'I'.
    l_hkont-option = 'BT'.
    l_hkont-low = '2221030504'.
    l_hkont-high = '2221030599'.
    APPEND l_hkont.

    l_hkont2-sign = 'I'.
    l_hkont2-option = 'BT'.
    l_hkont2-low = '1122010101'.
    l_hkont2-high = '1122010399'.
    APPEND l_hkont2.
    l_hkont3-sign = 'I'.
    l_hkont3-option = 'EQ'.
    l_hkont3-low = '1406010101'.  " l_hkont3  it02 150602 add
    APPEND l_hkont3.
    l_hkont4-sign = 'I'.
    l_hkont4-option = 'BT'.      " l_hkont4 it02 150602 add
    l_hkont4-low = '6001010101'.
    l_hkont4-high = '6001999999'.
    APPEND l_hkont4 .
    l_hkont5-sign = 'I'.
    l_hkont5-option = 'BT'.      " l_hkont5 it02 150602 add
    l_hkont5-low = '2221030501'.
    l_hkont5-high = '2221030599'.
    APPEND l_hkont5 .

    IF bseg-hkont IN l_hkont OR bseg-hkont IN l_hkont2.
      IF bseg-hkont IN l_hkont.
        bseg-hkont = '2221030503'.
      ELSEIF bseg-hkont IN l_hkont2.
        bseg-hkont = '1122010301'.
      ENDIF.

    ENDIF.
    "150602 it02 begin 生成会计凭证时，会计科目段为2221030501 – 2221030599 （销项税科目）
    "、1406010101（工程成本）和6001010101-6001999999（收入科目）的分配字段（BSEG- ZUONR）都需要替代为销售订单号
    IF bseg-hkont IN l_hkont5 OR bseg-hkont IN l_hkont4 OR bseg-hkont IN l_hkont3.
      IF bkpf-xblnr <> '' .
        bseg-zuonr = bkpf-xblnr.
      ENDIF.
    ENDIF.

*&--代码添加 BY HANDYBY 07.06.2017 17:50:29  BEGIN
    IF bseg-hkont = '1122020101'.
      bseg-hkont = '1122020201'.
    ENDIF.
*&--代码添加 BY HANDYBY 07.06.2017 17:50:29  END

    "150602 end it02
    CLEAR: l_hkont[],l_hkont2[].
  ENDIF.


  IF sy-tcode = 'MIGO' OR sy-tcode = 'MB1A'.
* "150521 注释begin
*   CLEAR l_flag.
*    "通过badi增强ZAC_DOC 取得当前过账是否为231/2 特殊库存标识 为空或者e
*    GET PARAMETER ID '2312E_FLAG' FIELD l_flag.
*    "对于按单无法替换 通过隐式增强：ZAC2312E_SUBSTITUTION 按单会计凭证参考销售单号结算行替代 解决
*    IF l_flag <> ''.
*      IF bseg-hkont BETWEEN '6404000000' AND '6464999999' AND bseg-vbel2 <> ''.
*        SELECT SINGLE vbeln auart
*          FROM vbak
*          INTO CORRESPONDING FIELDS OF lw_vbak
*          WHERE vbeln = bseg-vbel2.
*        IF sy-subrc = 0.
*          IF lw_vbak-auart = 'ZPO'
*            OR  lw_vbak-auart = 'ZF1'
*            OR  lw_vbak-auart = 'ZZG'
*            OR  lw_vbak-auart = 'ZWV'.
*            bseg-posn2 = '000100'.
*          ELSEIF  lw_vbak-auart = 'ZSO'.
*            bseg-posn2 = '000010'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    "150521 注释 END
    "150527 IT02 BEGIN
    CLEAR l_flag.
    "通过badi增强ZAC_DOC 取得当前过账是否为231/2 特殊库存标识 为空或者e
    GET PARAMETER ID '2312E_FLAG' FIELD l_flag.
    "对于按单无法替换 通过隐式增强：ZAC2312E_SUBSTITUTION 按单会计凭证参考销售单号结算行替代 解决
    IF l_flag <> ''.
      IF bseg-hkont BETWEEN '6404000000' AND '6404999999' AND bseg-vbel2 <> ''.
        SELECT SINGLE vbeln auart
          FROM vbak
          INTO CORRESPONDING FIELDS OF lw_vbak
          WHERE vbeln = bseg-vbel2.
        IF sy-subrc = 0.
          SELECT  vbeln
               posnr
               pstyv
               uepos
          FROM vbap
          INTO CORRESPONDING FIELDS OF TABLE lt_vbap
           WHERE vbeln = lw_vbak-vbeln .
          IF lt_vbap[] IS  NOT INITIAL.
            SORT lt_vbap BY vbeln posnr .
          ENDIF.
          IF lw_vbak-auart = 'ZPO'
            OR  lw_vbak-auart = 'ZF1'
            OR  lw_vbak-auart = 'ZZG'
            OR  lw_vbak-auart = 'ZWV'.
            LOOP AT lt_vbap INTO lw_vbap .
              CASE lw_vbap-posnr.
                WHEN '000100'.
                  posnrtmp = '000100' .
                  EXIT .
                WHEN '000200'.
                  posnrtmp = '000200' .
                  EXIT.
                WHEN '000300'.
                  posnrtmp = '000300' .
                  EXIT.
                WHEN '000400'.
                  posnrtmp = '000400' .
                  EXIT.
                WHEN '000500'.
                  posnrtmp = '000500' .
                  EXIT.
                WHEN '000600'.
                  posnrtmp = '000600' .
                  EXIT.
                WHEN '000700'.
                  posnrtmp = '000700' .
                  EXIT.
                WHEN '000800'.
                  posnrtmp = '000800' .
                  EXIT.
                WHEN '000900'.
                  posnrtmp = '000900' .
                  EXIT.
                WHEN '001000'.
                  posnrtmp = '001000' .
                  EXIT.
              ENDCASE.
            ENDLOOP.
            bseg-posn2 = posnrtmp.
          ELSEIF  lw_vbak-auart = 'ZSO'.
            LOOP AT lt_vbap INTO lw_vbap .
              CASE lw_vbap-posnr.
                WHEN '000010'.
                  posnrtmp = '000010' .
                  EXIT.
                WHEN '000020'.
                  posnrtmp = '000020' .
                  EXIT.
                WHEN '000030'.
                  posnrtmp = '000030' .
                  EXIT.
                WHEN '000040'.
                  posnrtmp = '000040' .
                  EXIT.
                WHEN '000050'.
                  posnrtmp = '000050' .
                  EXIT.
                WHEN '000060'.
                  posnrtmp = '000060' .
                  EXIT.
                WHEN '000070'.
                  posnrtmp = '000070' .
                  EXIT.
                WHEN '000080'.
                  posnrtmp = '000080' .
                  EXIT.
                WHEN '000090'.
                  posnrtmp = '000090' .
                  EXIT.
                WHEN '000100'.
                  posnrtmp = '000100' .
                  EXIT.
              ENDCASE.
            ENDLOOP.
            bseg-posn2 = posnrtmp.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  "150527 IT02 END
  "150521 IT02 BEGING .
*    CLEAR l_flag.
*    "通过badi增强ZAC_DOC 取得当前过账是否为231/2 特殊库存标识 为空或者e
*    GET PARAMETER ID '2312E_FLAG' FIELD l_flag.
*    "对于按单无法替换 通过隐式增强：ZAC2312E_SUBSTITUTION 按单会计凭证参考销售单号结算行替代 解决
*    IF l_flag <> ''.
*      IF bseg-hkont BETWEEN '6404000000' AND '6464999999' AND bseg-vbel2 <> ''.
*
*        SELECT SINGLE vbeln auart
*          FROM vbak
*          INTO CORRESPONDING FIELDS OF lw_vbak
*          WHERE vbeln = bseg-vbel2.
*        IF sy-subrc = 0.
*           "IT02 INSERT 150521 BEGIN
*           SELECT single vbeln
*                posnr
*                pstyv
*                uepos
*           FROM vbap
*           INTO CORRESPONDING FIELDS OF  lw_vbap
*           WHERE vbeln = lw_vbak-vbeln
*           AND   posnr = bseg-posn2
*           AND   pstyv IN ( 'Z03',
*                            'Z04',
*                            'Z05',
*                            'Z06',
*                            'Z07',
*                            'Z08',
*                            'Z09',
*                            'Z33',
*                            'Z34',
*                            'Z43',
*                            'Z44',
*                            'Z45',
*                            'Z46',
*                            'Z47',
*                            'Z48' ) .
*
*
*        "IT02 INSERT 150521 END
*          IF lw_vbak-auart = 'ZPO'
*            OR  lw_vbak-auart = 'ZF1'
*            OR  lw_vbak-auart = 'ZZG'
*            OR  lw_vbak-auart = 'ZWV'.
*          "  bseg-posn2 = '000100'.  IT02   标不可用 150521
*             bseg-posn2 = lw_vbap-uepos. "现更改为上一层的项目号 IT02 INSERT
*
*          ELSEIF  lw_vbak-auart = 'ZSO'.
*         "   bseg-posn2 = '000010'. IT02   标不可用 150521
*            bseg-posn2 = lw_vbap-uepos. "现更改为上一层的项目号 IT02 INSERT  150521
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
  "150521 IT02 END
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  SELECT * FROM CSKS
*            WHERE KOSTL EQ COBL-KOSTL
*              AND KOKRS EQ COBL-KOKRS.
*    IF CSKS-DATBI >= SY-DATUM AND
*       CSKS-DATAB <= SY-DATUM.
*
*      MOVE CSKS-ABTEI TO COBL-KOSTL.
*
*    ENDIF.
*  ENDSELECT.
  " ADD  深圳散屏发货包装订单 结算 字段替代 BEGIN 150702
  DATA: gt_afko LIKE TABLE OF afko WITH HEADER LINE.
  IF sy-tcode = 'KO88'.
    IF bkpf-blart = 'SC' AND bseg-hkont = '6401020101'.
      SELECT  * INTO CORRESPONDING FIELDS OF TABLE  gt_afko FROM afko WHERE aufnr = bseg-zuonr.
      IF gt_afko[] IS NOT INITIAL.
        SORT gt_afko BY aufnr.
        READ TABLE gt_afko INDEX 1.
        IF sy-subrc = 0.
          bseg-ebeln = gt_afko-cy_seqnr+4(10)."采购订单替换 顺序编号的后10位
          bseg-zuonr = gt_afko-cy_seqnr+4(10) ."分配替换 顺序编号的后10位
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  "ADD  深圳散屏发货包装订单 结算 字段替代 END 150702

*分配字段替代WBS元素  ADD BY HANDWY 2015-7-23
  DATA l_projk TYPE string.

*&--代码注释 BY HANDYBY 22.05.2017 17:40:28  BEGIN
*  IF BSEG-BUKRS = '1800'
*&--代码注释 BY HANDYBY 22.05.2017 17:40:28  END
*&--代码添加 BY HANDYBY 22.05.2017 17:40:34  BEGIN
  IF ( bseg-bukrs  = '1800' OR bseg-bukrs  = '1700' OR bseg-bukrs  = '1710' OR
       bseg-bukrs  = '2000' OR bseg-bukrs  = '2300' OR bseg-bukrs  = '2310' OR
       bseg-bukrs  = '2320' OR bseg-bukrs  = '2700' OR bseg-bukrs  = '2800' OR
       bseg-bukrs  = '3000' )
*&--代码添加 BY HANDYBY 22.05.2017 17:40:34  END

AND (
    bseg-hkont+0(4) = '1121'
OR  bseg-hkont+0(4) = '1122'
OR  bseg-hkont+0(4) = '1123'
OR  bseg-hkont+0(4) = '1221'
OR  bseg-hkont+0(4) = '2201'
OR  bseg-hkont+0(4) = '2202'
OR  bseg-hkont+0(4) = '2203'
OR  bseg-hkont+0(4) = '2241'
*&--代码添加 BY HANDYBY 24.07.2017 10:06:17  BEGIN
*  OR BSEG-HKONT = '5402010101'
*&--代码添加 BY HANDYBY 24.07.2017 10:06:17  END
)
 .
*   .
    IF  bseg-projk IS INITIAL.
      l_projk = bseg-zuonr.

*转换WBS元素内码
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = l_projk
        IMPORTING
          output    = bseg-projk
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
        MESSAGE s006(z001) DISPLAY LIKE 'I'.
      ENDIF.
    ENDIF.

  ENDIF.
  "增加物料帐6404科目根据网络号替代WBS和分配字段
  "20171115 by it02 业务：黄键杭
  IF bkpf-bukrs = '1700' AND bkpf-blart = 'ML'
    AND bseg-hkont(4) = '6404'.
    IF bseg-nplnr IS NOT INITIAL.
      SELECT SINGLE pspel
        INTO bseg-projk
        FROM aufk
        WHERE aufnr = bseg-nplnr.
      IF sy-subrc = 0.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
          EXPORTING
            input         = bseg-projk
          IMPORTING
            OUTPUT        = bseg-zuonr
                  .
*        bseg-zuonr = bseg-projk.
        CLEAR bseg-nplnr.
      ENDIF.
    ENDIF.
  ENDIF.
*ENDADD
ENDFORM.                                                    "U100
* eject
*---------------------------------------------------------------------*
*       FORM U101                                                     *
*---------------------------------------------------------------------*
*       Reads the cost-center from the CSKS table for accounting      *
*       area '0001'.                                                  *
*       This exit uses a parameter for the cost_center so it can      *
*       be used irrespective of the table used in the callup point.   *
*---------------------------------------------------------------------*
FORM u101 USING cost_center.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  SELECT * FROM CSKS
*            WHERE KOSTL EQ COST_CENTER
*              AND KOKRS EQ '0001'.
*    IF CSKS-DATBI >= SY-DATUM AND
*       CSKS-DATAB <= SY-DATUM.
*
*      MOVE CSKS-ABTEI TO COST_CENTER .
*
*    ENDIF.
*  ENDSELECT.

ENDFORM.                                                    "U101

* eject
*---------------------------------------------------------------------*
*       FORM U102                                                     *
*---------------------------------------------------------------------*
*       Inserts the sum of the posting into the reference field.      *
*       This exit can be used in FI for the complete document.        *
*       The complete data is passed in one parameter.                 *
*---------------------------------------------------------------------*
FORM u102.
*分配字段替代WBS元素  ADD BY HANDWY 2015-7-23
  DATA l_projk TYPE string.
*  IF BSEG-BUKRS = '1800'
*  AND ( ( BSEG-HKONT BETWEEN '1121010101'
*  AND '1122990101')
*  OR  BSEG-HKONT+0(4) = '1123'
*  OR  BSEG-HKONT+0(4) = '2203')
*   .
  IF  bseg-projk IS INITIAL.
    l_projk = bseg-zuonr.

*转换WBS元素内码
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
      EXPORTING
        input     = l_projk
      IMPORTING
        output    = bseg-projk
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE s006(z001) DISPLAY LIKE 'I'.
    ENDIF.
  ENDIF.
*  ENDIF.
*ENDADD
ENDFORM.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*FORM u102 USING bool_data TYPE gb002_015.
*DATA: SUM(10) TYPE C.
*
*    LOOP AT BOOL_DATA-BSEG INTO BSEG
*                    WHERE    SHKZG = 'S'.
*       BSEG-ZUONR = 'Test'.
*       MODIFY BOOL_DATA-BSEG FROM BSEG.
*       ADD BSEG-DMBTR TO SUM.
*    ENDLOOP.
*
*    BKPF-XBLNR = TEXT-001.
*    REPLACE '&' WITH SUM INTO BKPF-XBLNR.
*
*ENDFORM.


***********************************************************************
** EXIT EXAMPLES FROM PUBLIC SECTOR INDUSTRY SOLUTION
**
** PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINE
** TO ENABLE PUBLIC SECTOR EXAMPLE SUBSTITUTION EXITS
***********************************************************************
*INCLUDE rggbs_ps_forms.


*eject
* begin of insertion                                          "wms092357
*&---------------------------------------------------------------------*
*&      Form  u200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM u200 USING e_rmvct TYPE bseg-bewar.
  PERFORM xref_to_rmvct USING bkpf bseg 1 CHANGING e_rmvct.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  xref_to_rmvct
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM xref_to_rmvct
     USING    is_bkpf         TYPE bkpf
              is_bseg         TYPE bseg
              i_xref_field    TYPE i
     CHANGING c_rmvct         TYPE rmvct.

  DATA l_msgv TYPE symsgv.
  STATICS st_rmvct TYPE HASHED TABLE OF rmvct WITH UNIQUE DEFAULT KEY.

* either bseg-xref1 or bseg-xref2 must be used as source...
  IF i_xref_field <> 1 AND i_xref_field <> 2.
    MESSAGE x000(gk) WITH 'UNEXPECTED VALUE I_XREF_FIELD ='
      i_xref_field '(MUST BE = 1 OR = 2)' ''.
  ENDIF.
  IF st_rmvct IS INITIAL.
    SELECT trtyp FROM t856 INTO TABLE st_rmvct.
  ENDIF.
  IF i_xref_field = 1.
    c_rmvct = is_bseg-xref1.
  ELSE.
    c_rmvct = is_bseg-xref2.
  ENDIF.
  IF c_rmvct IS INITIAL.
    WRITE i_xref_field TO l_msgv LEFT-JUSTIFIED.
    CONCATENATE text-m00 l_msgv INTO l_msgv SEPARATED BY space.
*   cons. transaction type is not specified => send an error message...
    MESSAGE e123(g3) WITH l_msgv.
*   Bitte geben Sie im Feld &1 eine Konsolidierungsbewegungsart an
  ENDIF.
* c_rmvct <> initial...
  READ TABLE st_rmvct TRANSPORTING NO FIELDS FROM c_rmvct.
  CHECK NOT sy-subrc IS INITIAL.
* cons. transaction type does not exist => send error message...
  WRITE i_xref_field TO l_msgv LEFT-JUSTIFIED.
  CONCATENATE text-m00 l_msgv INTO l_msgv SEPARATED BY space.
  MESSAGE e124(g3) WITH c_rmvct l_msgv.
* KonsBewegungsart &1 ist ungültig (bitte Eingabe im Feld &2 korrigieren
ENDFORM.

*  应付暂估规则统一文本及分配
FORM u300.
  DATA:lv_lifnr TYPE ekko-lifnr,
       lv_vbeln TYPE ekkn-vbeln.
  IF bseg-ebeln IS NOT INITIAL.
    SELECT SINGLE lifnr
      INTO lv_lifnr
      FROM ekko
      WHERE ebeln = bseg-ebeln.

    IF sy-subrc = 0.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_lifnr   " Any ABAP field
        IMPORTING
          output = lv_lifnr.   " External INPUT display, C field
      bseg-zuonr = lv_lifnr.
    ENDIF.

    SELECT SINGLE vbeln
      INTO lv_vbeln
      FROM ekkn
      WHERE ebeln = bseg-ebeln
      AND   ebelp = bseg-ebelp.

    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_vbeln    " Any ABAP field
        IMPORTING
          output = lv_vbeln.    " External INPUT display, C field

      bseg-sgtxt = lv_vbeln.
    ENDIF.

  ENDIF.

ENDFORM.
