class ZIF_EX_ME_CHANGE_OUTTAB_CUS definition
  public
  final
  create public .

*"* public components of class ZIF_EX_ME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_CHANGE_OUTTAB_CUS .
protected section.
*"* protected components of class ZIF_EX_ME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
private section.
*"* private components of class ZIF_EX_ME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZIF_EX_ME_CHANGE_OUTTAB_CUS IMPLEMENTATION.


METHOD if_ex_me_change_outtab_cus~fill_outtab.

  FIELD-SYMBOLS:<fs_outtab> TYPE any.
  FIELD-SYMBOLS:<fs_any>    TYPE any.
  DATA:gt_ekko     TYPE TABLE OF ekko.
  DATA:gs_ekpo     TYPE ekpo.
  DATA:gs_ekko     TYPE ekko.
  DATA:gt_ekpo     TYPE TABLE OF ekpo.
  DATA:gt_ekpo_ekkn     TYPE TABLE OF ekpo.
  DATA: gs_konv TYPE konv,
        gt_konv TYPE TABLE OF konv.

*审批报表
  IF im_struct_name = 'MEREP_OUTTAB_PURCHDOC_REL'
    AND im_id = 'RM06EF00'.

    DATA:ls_outtab   TYPE merep_outtab_purchdoc.
    DATA:ls_outtab_1 TYPE merep_outtab_purchdoc_rel.
    DATA:ls_ekkn TYPE ekkn.
    DATA:ls_afpo TYPE afpo.
    DATA:ls_makt TYPE makt.
    DATA:gt_eine     TYPE TABLE OF eine.
    DATA:gs_eine     TYPE eine.
    DATA:gt_eina     TYPE TABLE OF eina.
    DATA:gs_eina     TYPE eina.
*    DATA:gt_ekko     TYPE TABLE OF ekko.
*    DATA:gs_ekpo     TYPE ekpo.
*    DATA:gs_ekko     TYPE ekko.
*    DATA:gt_ekpo     TYPE TABLE OF ekpo.
    DATA l_esokz     TYPE eine-esokz.
    DATA gs_dd07t    TYPE dd07t.
    "  DATA gs_konv     TYPE konv.
    DATA:gt_zmm024   TYPE TABLE OF zmm024 .
    DATA:gs_zmm024   TYPE zmm024 .
    DATA:gs_mara     TYPE mara.
    DATA:gt_t023t TYPE TABLE OF t023t,
         gs_t023t TYPE t023t.
    DATA:gt_lfa1 TYPE TABLE OF lfa1,
         gs_lfa1 TYPE lfa1.
*    IF SY-TCODE = 'ME28'.
    LOOP AT ch_outtab ASSIGNING <fs_outtab>.
      CLEAR:ls_outtab_1.

      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_1-ebeln = <fs_any>.

      ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_1-ebelp = <fs_any>.

      ASSIGN COMPONENT 'EMATN' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_1-ematn = <fs_any>.

      ASSIGN COMPONENT 'EKORG' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_1-ekorg = <fs_any>.

      ASSIGN COMPONENT 'WERKS' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_1-werks = <fs_any>.

      ASSIGN COMPONENT 'PSTYP' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_1-pstyp = <fs_any>.

      ASSIGN COMPONENT 'ZKBETR' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_1-zkbetr = <fs_any>.

*检查料号是否存在
      CHECK ls_outtab_1-ematn IS NOT INITIAL .

      CLEAR gs_ekko.
      SELECT SINGLE * FROM ekko INTO
      CORRESPONDING FIELDS OF gs_ekko
      WHERE ebeln = ls_outtab_1-ebeln.

      REFRESH gt_eina.
      SELECT * FROM eina INTO
       CORRESPONDING FIELDS OF TABLE gt_eina
       WHERE lifnr = gs_ekko-lifnr
       AND   matnr = ls_outtab_1-ematn.

      IF gt_eina IS NOT INITIAL.
        CLEAR l_esokz.
        READ TABLE gt_eina INTO gs_eina
        INDEX 1.
        IF sy-subrc = 0 .
          IF ls_outtab_1-pstyp = '0'.
            l_esokz = '0'.
          ELSEIF ls_outtab_1-pstyp = '3'..
            l_esokz = '3'.
          ENDIF.
        ENDIF.

*采购信息记录价格比较标识

*当工厂级别没有取到信息记录，去取采购组织级别
        CLEAR gs_eine.
        SELECT SINGLE * FROM eine INTO
        CORRESPONDING FIELDS OF  gs_eine
         WHERE ekorg =  ls_outtab_1-ekorg
         AND   werks =  ls_outtab_1-werks
         AND   infnr =  gs_eina-infnr
         AND   esokz =  l_esokz.

        IF sy-subrc <> 0.
          CLEAR gs_eine.
          SELECT SINGLE * FROM eine INTO
          CORRESPONDING FIELDS OF  gs_eine
           WHERE ekorg =  ls_outtab_1-ekorg
           AND   infnr =  gs_eina-infnr
           AND   esokz =  l_esokz.
        ENDIF.

        IF gs_eine-netpr <> 0.
*高于信息记录
          IF gs_eine-netpr < ls_outtab_1-zkbetr.
            ls_outtab_1-zflag = '2'.
          ENDIF.

*小于信息记录
          IF gs_eine-netpr > ls_outtab_1-zkbetr.
            ls_outtab_1-zflag = '3'.
          ENDIF.

*等于信息记录
          IF gs_eine-netpr = ls_outtab_1-zkbetr.
            ls_outtab_1-zflag = '4'.
          ENDIF.
        ELSE.
          ls_outtab_1-zflag = '1'.
        ENDIF.

*上次采购价格比较标识

        IF gs_eine-ebeln IS INITIAL.
          ls_outtab_1-zflag1 = '1'.
        ENDIF.

        IF gs_eine-ebeln IS NOT INITIAL
         OR gs_eine-ebelp IS NOT INITIAL.

*查询信息记录上采购订单含税价格
          SELECT SINGLE * FROM ekpo
          INTO CORRESPONDING FIELDS OF gs_ekpo
          WHERE ebeln = gs_eine-ebeln
          AND   ebelp = gs_eine-ebelp.

          CLEAR gs_konv.
          SELECT SINGLE * FROM konv
           INTO CORRESPONDING FIELDS OF  gs_konv
           WHERE knumv = gs_ekko-knumv
           AND   kposn = gs_ekpo-ebelp
           AND   ( kschl = 'PB00'
           OR    kschl   = 'PBXX').

*高于上次采购价格
          IF gs_konv-kbetr < ls_outtab_1-zkbetr.
            ls_outtab_1-zflag1 = '2'.
          ENDIF.

*低于上次采购价格
          IF gs_konv-kbetr > ls_outtab_1-zkbetr.
            ls_outtab_1-zflag1 = '3'.
          ENDIF.

*等于上次采购价格
          IF gs_konv-kbetr = ls_outtab_1-zkbetr.
            ls_outtab_1-zflag1 = '4'.
          ENDIF.

        ENDIF.

      ELSE.
        ls_outtab_1-zflag = '1'.
        ls_outtab_1-zflag1 = '1'.
      ENDIF.

      ASSIGN COMPONENT 'ZFLAG' OF STRUCTURE <fs_outtab> TO <fs_any>.
      <fs_any> = ls_outtab_1-zflag.

*添加标识描述
      CLEAR gs_dd07t.
      SELECT SINGLE * FROM dd07t
      INTO CORRESPONDING FIELDS OF gs_dd07t
      WHERE domname     =  'ZFLAG'
      AND   valpos      = ls_outtab_1-zflag
      AND   ddlanguage  = sy-langu.
      IF sy-subrc = 0.
        ls_outtab_1-zflag_t = gs_dd07t-ddtext.

        ASSIGN COMPONENT 'ZFLAG_T' OF STRUCTURE <fs_outtab> TO <fs_any>.
        <fs_any> = ls_outtab_1-zflag_t.
      ENDIF.


      ASSIGN COMPONENT 'ZFLAG1' OF STRUCTURE <fs_outtab> TO <fs_any>.
      <fs_any> = ls_outtab_1-zflag1.

*添加标识描述
      CLEAR gs_dd07t.
      SELECT SINGLE * FROM dd07t
      INTO CORRESPONDING FIELDS OF gs_dd07t
      WHERE domname     =  'ZFLAG1'
      AND   valpos      = ls_outtab_1-zflag1
      AND   ddlanguage  = sy-langu.
      IF sy-subrc = 0.
        ls_outtab_1-zflag1_t = gs_dd07t-ddtext.

        ASSIGN COMPONENT 'ZFLAG1_T' OF STRUCTURE <fs_outtab> TO <fs_any>.
        <fs_any> = ls_outtab_1-zflag1_t.
      ENDIF.
    ENDLOOP.
*    ENDIF.
  ENDIF.

  DATA lt_tline    TYPE TABLE OF tline.
  DATA ls_tline    TYPE tline.
  DATA l_longtext  TYPE thead-tdname.
  DATA l_longtext1 TYPE thead-tdname.

  "ME2N：采购凭证明细增强
  IF im_struct_name = 'MEREP_OUTTAB_PURCHDOC'
    AND ( im_id = 'RM06EL00'  OR im_id = 'RM06EM00'
    OR im_id = 'RM06EN00').

    TYPES :BEGIN OF ty_cgdd,
             ebeln TYPE ekpo-ebeln,
             ebelp TYPE ekpo-ebelp,
           END OF ty_cgdd.


    TYPES:BEGIN OF ty_cgsq,
            banfn TYPE eban-banfn,  "采购申请号
            bnfpo TYPE eban-bnfpo,  "采购申请的项目编号
          END OF ty_cgsq.

    TYPES:BEGIN OF ty_xmh,
            psphi TYPE prps-psphi,
          END OF ty_xmh.

    TYPES:BEGIN OF ty_matnr,
            ematn TYPE mara-matnr,  "物料号
          END OF ty_matnr.

    DATA:gt_cgdd TYPE TABLE OF ty_cgdd,  "采购订单
         gs_cgdd TYPE ty_cgdd.

    DATA:gt_ematn TYPE TABLE OF ty_matnr, "物料号
         gs_ematn TYPE ty_matnr.

    DATA:gt_cgsq TYPE TABLE OF ty_cgsq,  "采购申请
         gs_cgsq TYPE ty_cgsq.

    DATA:gt_xmh TYPE TABLE OF ty_xmh,  "项目号
         gs_xmh TYPE ty_xmh.

    DATA:gt_ebkn TYPE TABLE OF ebkn,  "采购申请账户设置
         gs_ebkn TYPE ebkn.


    DATA:gt_prps TYPE TABLE OF prps,  "WBS元素
         gs_prps TYPE prps.

    DATA:gt_proj TYPE TABLE OF proj, "项目名称
         gs_proj TYPE proj.

    DATA:gt_mara TYPE TABLE OF mara. "物料号

    DATA:t_banfn TYPE ebkn-banfn,
         t_bnfpo TYPE ebkn-bnfpo,
         t_matnr TYPE mara-matnr.

    DATA:gt_ekko_mx TYPE TABLE OF ekko, "采购明细
         gs_ekko_mx TYPE ekko.

    DATA:l_year TYPE n LENGTH 4.       "哪个年度
    DATA:l_date_l TYPE d,               "上年度最小日期
         l_date_h TYPE d .               "上年度最大日期

    DATA:gt_ekpo_l TYPE TABLE OF ekpo,   "上年度采购明细
         gs_ekpo_l TYPE ekpo.

    DATA:gt_ekko_l TYPE TABLE OF ekko,   "上年度采购订单
         gs_ekko_l TYPE ekko.

    DATA:gt_ekko_l_mx TYPE TABLE OF ekko, "上年度采购订单明细
         gs_ekko_l_mx TYPE ekko.

    DATA:gt_konv_l TYPE TABLE OF konv,    "上年度采购定价
         gs_konv_l TYPE konv.

    DATA:gt_ekbe TYPE TABLE OF ekbe,      "采购历史
         gs_ekbe TYPE ekbe.
    DATA: lt_ekkn TYPE TABLE OF ekkn.     "采购订单账号分配

    "

*    IF SY-TCODE = 'ME2L'.

    "
    IF ch_outtab IS NOT INITIAL.

      "检索不同物料号
      MOVE-CORRESPONDING ch_outtab TO gt_ematn.
      SORT gt_ematn BY ematn .
      DELETE  gt_ematn WHERE ematn EQ ''.
      DELETE ADJACENT DUPLICATES FROM  gt_ematn  COMPARING ematn.

      "检索采购凭证明细
      MOVE-CORRESPONDING ch_outtab TO gt_cgdd.
      SORT gt_cgdd BY ebeln ebelp .

      "检索采购凭证明细数据
      IF gt_cgdd IS NOT INITIAL.
        SELECT * INTO TABLE gt_ekpo
       FROM ekpo
       FOR ALL ENTRIES IN  gt_cgdd
       WHERE ebeln = gt_cgdd-ebeln
       AND ebelp = gt_cgdd-ebelp.
        SORT gt_ekpo BY ebeln ebelp.


        IF gt_ekpo IS NOT INITIAL.
          "采购订单账户分配信息
          gt_ekpo_ekkn[] = gt_ekpo[].
          DELETE gt_ekpo_ekkn WHERE banfn IS NOT INITIAL.
          IF gt_ekpo_ekkn[] IS NOT INITIAL.
            SELECT *
              FROM ekkn
              INTO TABLE lt_ekkn
              FOR ALL ENTRIES IN gt_ekpo_ekkn
              WHERE ebeln = gt_ekpo_ekkn-ebeln
              AND   ebelp = gt_ekpo_ekkn-ebelp.
            IF sy-subrc = 0.
              SORT lt_ekkn BY ebeln ebelp ps_psp_pnr.
              DELETE ADJACENT DUPLICATES FROM lt_ekkn COMPARING ebeln ebelp ps_psp_pnr.
            ENDIF.
          ENDIF.
          "检索采购申请分类
          MOVE-CORRESPONDING gt_ekpo TO gt_cgsq.
          SORT gt_cgsq BY banfn bnfpo.
          DELETE ADJACENT DUPLICATES FROM gt_cgsq COMPARING banfn bnfpo.

          ""检索采购抬头
          REFRESH:gt_ekko.
          MOVE-CORRESPONDING gt_ekpo TO gt_ekko.
          SORT gt_ekko BY ebeln.
          DELETE ADJACENT DUPLICATES FROM gt_ekko COMPARING ebeln.

          "检索采购抬头明细
          SELECT * INTO TABLE gt_ekko_mx
            FROM ekko
            FOR ALL ENTRIES IN gt_ekko
            WHERE ebeln = gt_ekko-ebeln.
          SORT gt_ekko BY ebeln.

          "检索采购明细的定价
          SELECT * INTO TABLE gt_konv
          FROM konv
          FOR ALL ENTRIES IN gt_ekko_mx
          WHERE knumv = gt_ekko_mx-knumv AND  ( kschl = 'PBXX' OR   kschl = 'PB00')
                AND kinak = ''.

          SORT gt_konv BY knumv kposn.


        ENDIF.




        "查询最后收货日期
        SELECT * INTO TABLE gt_ekbe
          FROM ekbe
          FOR ALL ENTRIES IN gt_cgdd
          WHERE ebeln = gt_cgdd-ebeln
          AND ebelp = gt_cgdd-ebelp
          AND bwart IN ('101','105').
        SORT gt_ekbe BY ebeln ASCENDING ebelp ASCENDING budat DESCENDING.
        DELETE ADJACENT DUPLICATES FROM gt_ekbe COMPARING ebeln ebelp .
      ENDIF.
      "根据采购申请号查询采购申请账户设置信息
      IF gt_cgsq IS NOT INITIAL.
        SELECT * INTO TABLE gt_ebkn
         FROM ebkn
         FOR ALL ENTRIES IN gt_cgsq
         WHERE banfn = gt_cgsq-banfn
         AND   bnfpo = gt_cgsq-bnfpo .
        SORT gt_ebkn BY banfn bnfpo.

        IF gt_ebkn IS NOT INITIAL.
          "根据采购申请账户的WBS号查询ＷＢＳ相关信息
          SELECT * INTO TABLE gt_prps
             FROM prps
             FOR ALL ENTRIES IN gt_ebkn
             WHERE pspnr = gt_ebkn-ps_psp_pnr.

          SORT gt_prps BY pspnr.
        ENDIF.
        IF lt_ekkn[] IS NOT INITIAL.
          "根据采购订单账户的WBS号查询ＷＢＳ相关信息
          SELECT * APPENDING TABLE gt_prps
             FROM prps
             FOR ALL ENTRIES IN lt_ekkn
             WHERE pspnr = lt_ekkn-ps_psp_pnr.

          SORT gt_prps BY pspnr.
        ENDIF.
        "检索出不同的项目号
        MOVE-CORRESPONDING  gt_prps TO gt_xmh.
        SORT gt_xmh BY psphi.
        DELETE ADJACENT DUPLICATES FROM gt_xmh COMPARING psphi.

        "查询项目名称
        IF gt_xmh[] IS NOT INITIAL.
          SELECT * INTO TABLE gt_proj
            FROM proj
            FOR ALL ENTRIES IN gt_xmh
            WHERE pspnr = gt_xmh-psphi.
          SORT gt_proj BY  pspnr.

        ENDIF.


      ENDIF.

      "查询物料号
      IF gt_ematn IS NOT INITIAL.
        SELECT * INTO TABLE gt_mara
          FROM mara
          FOR ALL ENTRIES IN gt_ematn
          WHERE matnr = gt_ematn-ematn.
        SORT gt_mara BY matnr.


        "上一年值
        l_year = sy-datum+0(4) - 1.
        ""上年度最小日期
        CONCATENATE l_year '01' '01' INTO l_date_l.
        "上年度最大日期
        CONCATENATE l_year '12' '31' INTO l_date_h.

        "查询检索出的物料的上一年度的采购明细
        SELECT * INTO TABLE gt_ekpo_l
          FROM ekpo
          FOR ALL ENTRIES IN gt_ematn
          WHERE matnr = gt_ematn-ematn
          AND prdat BETWEEN l_date_l AND l_date_h
          AND loekz EQ ''.
        SORT gt_ekpo_l BY matnr ASCENDING  aedat DESCENDING.
        DELETE  ADJACENT DUPLICATES FROM gt_ekpo_l COMPARING matnr.

        "查询检索出的物料的上一年度的采购订单
        MOVE-CORRESPONDING gt_ekpo_l TO gt_ekko_l.
        SORT gt_ekko_l BY ebeln .
        DELETE ADJACENT DUPLICATES FROM gt_ekko_l COMPARING ebeln.

        IF gt_ekko_l IS NOT INITIAL.
          "查询检索出的物料的上一年度的采购订单明细
          SELECT  * INTO TABLE gt_ekko_l_mx
            FROM ekko
            FOR ALL ENTRIES IN gt_ekko_l
            WHERE ebeln = gt_ekko_l-ebeln.
          SORT gt_ekko_l_mx BY ebeln .

          ""查询检索出的物料的上一年度的采购订单定价
          SELECT * INTO TABLE gt_konv_l
            FROM konv
            FOR ALL ENTRIES IN gt_ekko_l_mx
            WHERE knumv = gt_ekko_l_mx-knumv AND  ( kschl = 'PBXX' OR   kschl = 'PB00') AND kinak = ''.

          SORT gt_konv_l BY knumv kposn.
        ENDIF.
      ENDIF.
    ENDIF.


    LOOP AT ch_outtab ASSIGNING <fs_outtab>.
      CLEAR:ls_outtab,t_matnr,l_longtext,l_longtext1,t_banfn , t_bnfpo .

      ASSIGN COMPONENT 'EMATN' OF STRUCTURE <fs_outtab> TO <fs_any>.
      IF sy-subrc EQ 0 .
        t_matnr = <fs_any>.
      ENDIF.


      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_outtab> TO <fs_any>.
      IF sy-subrc EQ 0.
        ls_outtab-ebeln = <fs_any>.
      ENDIF.

      ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_outtab> TO <fs_any>.
      IF sy-subrc EQ 0.
        ls_outtab-ebelp = <fs_any>.
      ENDIF.

      READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln =  ls_outtab-ebeln
                                               ebelp =  ls_outtab-ebelp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_outtab-afnam = gs_ekpo-afnam.
        ASSIGN COMPONENT 'AFNAM' OF STRUCTURE <fs_outtab> TO <fs_any>.
        IF sy-subrc EQ 0.
          <fs_any> = ls_outtab-afnam.
        ENDIF.
        t_banfn = gs_ekpo-banfn.
        t_bnfpo = gs_ekpo-bnfpo.
      ENDIF.

*      CLEAR gs_ekpo.
*      SELECT SINGLE * FROM ekpo
*      INTO CORRESPONDING FIELDS OF gs_ekpo
*      WHERE ebeln = ls_outtab-ebeln
*      AND   ebelp = ls_outtab-ebelp.
*
*      ls_outtab-afnam = gs_ekpo-afnam.
*
*      ASSIGN COMPONENT 'AFNAM' OF STRUCTURE <fs_outtab> TO <fs_any>.
*      <fs_any> = ls_outtab-afnam.

*获取备注文本
      CONCATENATE ls_outtab-ebeln ls_outtab-ebelp INTO l_longtext.

*获取采购申请
      CONCATENATE gs_ekpo-banfn gs_ekpo-bnfpo INTO l_longtext1.


      "备注
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'F01'
          language                = sy-langu
          name                    = l_longtext
          object                  = 'EKPO'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*   IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = lt_tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        LOOP AT lt_tline INTO ls_tline.
          CONCATENATE  ls_outtab-bz ls_tline-tdline INTO ls_outtab-bz SEPARATED BY space.
        ENDLOOP.
      ENDIF.

      ASSIGN COMPONENT 'BZ' OF STRUCTURE <fs_outtab> TO <fs_any>.
      IF sy-subrc EQ 0.
        <fs_any> = ls_outtab-bz.
      ENDIF.


*获取采购申请传送系统
      REFRESH lt_tline.
      CLEAR   ls_tline.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'B03'
          language                = sy-langu
          name                    = l_longtext1
          object                  = 'EBAN'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*   IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = lt_tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        LOOP AT lt_tline INTO ls_tline.
          CONCATENATE  ls_outtab-cgsqcswb ls_tline-tdline INTO ls_outtab-cgsqcswb SEPARATED BY space.
        ENDLOOP.
      ENDIF.

      ASSIGN COMPONENT 'CGSQCSWB' OF STRUCTURE <fs_outtab> TO <fs_any>.
      IF sy-subrc EQ 0.
        <fs_any> = ls_outtab-cgsqcswb.
      ENDIF.

      "含税价、含税值
      READ TABLE gt_ekko_mx INTO gs_ekko_mx WITH KEY ebeln = ls_outtab-ebeln .
      IF sy-subrc EQ 0.
        READ TABLE gt_konv INTO gs_konv WITH KEY knumv = gs_ekko_mx-knumv
                                                  kposn+1(5) =   ls_outtab-ebelp
                                                  BINARY SEARCH.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'ZHSZ' OF STRUCTURE <fs_outtab> TO <fs_any>.
          IF sy-subrc EQ 0 .
            <fs_any> = gs_konv-kwert.
          ENDIF.
          "含税价
          AUTHORITY-CHECK OBJECT 'M_BEST_BSA'
        ID 'ACTVT' FIELD '09'
        ID 'BSART' FIELD '09' .
          IF sy-subrc = 0.
            ASSIGN COMPONENT 'ZKBETR' OF STRUCTURE <fs_outtab> TO <fs_any>.
            IF sy-subrc EQ 0 .
              <fs_any> = gs_konv-kbetr.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

*      "add 增加采购报表的含税值：读取采购订单条件的含税值KWERT by it02_weiyun 20161118
*      CLEAR:gs_ekko.
*      SELECT SINGLE * INTO gs_ekko FROM ekko
*                                   WHERE ebeln = ls_outtab-ebeln .
*      IF sy-subrc EQ 0 .
*        CLEAR:gs_konv.
*        SELECT SINGLE * INTO gs_konv FROM konv
*                                     WHERE knumv = gs_ekko-knumv
*                                      AND  kposn = ls_outtab-ebelp
*                                      AND  kschl IN ( 'PB00' ,'PBXX' )
*                                      AND  kinak = ''.
*        IF sy-subrc EQ 0 .
*          ASSIGN COMPONENT 'ZHSZ' OF STRUCTURE <fs_outtab> TO <fs_any>.
*          IF sy-subrc EQ 0 .
*            <fs_any> = gs_konv-kwert.
*          ENDIF.
*          "含税价
*          AUTHORITY-CHECK OBJECT 'M_BEST_BSA'
*        ID 'ACTVT' FIELD '09'
*        ID 'BSART' FIELD '09' .
*          IF sy-subrc = 0.
*            ASSIGN COMPONENT 'ZKBETR' OF STRUCTURE <fs_outtab> TO <fs_any>.
*            IF sy-subrc EQ 0 .
*              <fs_any> = gs_konv-kbetr.
*            ENDIF.
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
      "根据检索出的采购申请、行号去查询WBS号、项目名称
      IF t_banfn IS NOT INITIAL.
        READ TABLE gt_ebkn INTO gs_ebkn WITH KEY banfn =  t_banfn
                                                bnfpo = t_bnfpo
                                                BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE gt_prps INTO gs_prps WITH KEY pspnr = gs_ebkn-ps_psp_pnr
                                                   BINARY SEARCH.
        ENDIF.
      ELSE.
        READ TABLE lt_ekkn INTO ls_ekkn WITH KEY ebeln = ls_outtab-ebeln
                                                 ebelp = ls_outtab-ebelp
                                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE gt_prps INTO gs_prps WITH KEY pspnr = ls_ekkn-ps_psp_pnr
                                                   BINARY SEARCH.
        ENDIF.
      ENDIF.


      IF sy-subrc EQ 0 .
        "检索wbs号
        ASSIGN COMPONENT 'ZPS_PSP_PNR' OF STRUCTURE <fs_outtab> TO <fs_any> .
        IF sy-subrc EQ 0.
          <fs_any> = gs_prps-posid.
        ENDIF.
        READ TABLE gt_proj INTO gs_proj WITH KEY  pspnr = gs_prps-psphi
                                                  BINARY SEARCH.
        IF sy-subrc EQ 0.
          "检索项目名称
          ASSIGN COMPONENT 'ZPOST1' OF STRUCTURE <fs_outtab> TO <fs_any> .
          IF sy-subrc EQ 0.
            <fs_any> = gs_proj-post1. "项目名称
          ENDIF.
        ENDIF.

      ENDIF.

      IF t_matnr IS NOT INITIAL.
        "检索品牌
        READ TABLE gt_mara INTO gs_mara WITH KEY matnr = t_matnr
                                       BINARY SEARCH.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT 'ZEXTWG' OF STRUCTURE <fs_outtab> TO <fs_any> .
          IF sy-subrc EQ 0.
            <fs_any>  = gs_mara-extwg.
          ENDIF.
        ENDIF.
        "查询物料的上一年度价

        READ TABLE gt_ekpo_l INTO gs_ekpo_l WITH KEY matnr = t_matnr
                                            BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE gt_ekko_l_mx INTO gs_ekko_l_mx WITH KEY ebeln =  gs_ekpo_l-ebeln
                                       BINARY SEARCH.
          IF sy-subrc EQ 0 .
            READ TABLE gt_konv_l INTO gs_konv_l WITH KEY knumv = gs_ekko_l_mx-knumv
                                                         kposn+1(5) =   gs_ekpo_l-ebelp
                                                         BINARY SEARCH.
            IF sy-subrc EQ 0.
              ASSIGN COMPONENT 'ZLS' OF STRUCTURE <fs_outtab> TO <fs_any> .
              IF sy-subrc EQ 0.
                <fs_any>  = gs_konv_l-kbetr .
              ENDIF.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.

      "查询最后收货日期
      READ TABLE gt_ekbe INTO gs_ekbe WITH KEY ebeln = ls_outtab-ebeln
                                               ebelp = ls_outtab-ebelp
                                               BINARY SEARCH.
      IF sy-subrc EQ 0 .
        ASSIGN COMPONENT 'ZBUDAT' OF STRUCTURE <fs_outtab> TO <fs_any> .
        IF sy-subrc EQ 0.
          <fs_any>  = gs_ekbe-budat .
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.
*    ENDIF.

  DATA:ls_outtab_2 TYPE merep_outtab_accounting.

  IF im_struct_name = 'MEREP_OUTTAB_ACCOUNTING'
  AND im_id = 'RM06EKPS'.
*    IF SY-TCODE = 'ME2J'.
    LOOP AT ch_outtab ASSIGNING <fs_outtab>.
      CLEAR:ls_outtab_2.

      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_2-ebeln = <fs_any>.

      ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_2-ebelp = <fs_any>.

*获取备注文本
*        CONCATENATE LS_OUTTAB-EBELN  INTO L_LONGTEXT.
      l_longtext = ls_outtab_2-ebeln.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'F06'
          language                = sy-langu
          name                    = l_longtext
          object                  = 'EKKO'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*   IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = lt_tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        LOOP AT lt_tline INTO ls_tline.
          CONCATENATE  ls_outtab_2-htbh ls_tline-tdline INTO ls_outtab_2-htbh SEPARATED BY space.
        ENDLOOP.
      ENDIF.

      ASSIGN COMPONENT 'HTBH' OF STRUCTURE <fs_outtab> TO <fs_any>.
      <fs_any> = ls_outtab_2-htbh.

      "add 增加采购报表的含税值：读取采购订单条件的含税值KWERT by it02_weiyun 20161118
      CLEAR:gs_ekko.
      SELECT SINGLE * INTO gs_ekko FROM ekko
                                   WHERE ebeln = ls_outtab_2-ebeln .
      IF sy-subrc EQ 0 .
        CLEAR:gs_konv.
        SELECT SINGLE * INTO gs_konv FROM konv
                                     WHERE knumv = gs_ekko-knumv
                                      AND  kposn = ls_outtab_2-ebelp
                                      AND  kschl IN ( 'PB00' ,'PBXX' )
                                      AND  kinak = ''..
        IF sy-subrc EQ 0 .
          ASSIGN COMPONENT 'ZHSZ' OF STRUCTURE <fs_outtab> TO <fs_any>.
          IF sy-subrc EQ 0 .
            <fs_any> = gs_konv-kwert.
          ENDIF.
          "含税价
          AUTHORITY-CHECK OBJECT 'M_BEST_BSA'
                              ID 'ACTVT' FIELD '09'
                              ID 'BSART' FIELD '09' .
          IF sy-subrc = 0.
            ASSIGN COMPONENT 'ZKBETR' OF STRUCTURE <fs_outtab> TO <fs_any>.
            IF sy-subrc EQ 0 .
              <fs_any> = gs_konv-kbetr.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
*    ENDIF.
  ENDIF.

  "事务码：ME2K.
  IF im_struct_name = 'MEREP_OUTTAB_ACCOUNTING'
     AND im_id = 'RM06EK00'.
    LOOP AT ch_outtab ASSIGNING <fs_outtab>.
      CLEAR:ls_outtab.

      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab-ebeln = <fs_any>.

      ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab-ebelp = <fs_any>.

      "add 增加采购报表的含税值：读取采购订单条件的含税值KWERT by it02_weiyun 20161118
      CLEAR:gs_ekko.
      SELECT SINGLE * INTO gs_ekko FROM ekko
                                   WHERE ebeln = ls_outtab-ebeln .
      IF sy-subrc EQ 0 .
        CLEAR:gs_konv.
        SELECT SINGLE * INTO gs_konv FROM konv
                                     WHERE knumv = gs_ekko-knumv
                                      AND  kposn = ls_outtab-ebelp
                                      AND  kschl IN ( 'PB00' ,'PBXX' )
                                      AND kinak = ''.
        IF sy-subrc EQ 0 .
          ASSIGN COMPONENT 'ZHSZ' OF STRUCTURE <fs_outtab> TO <fs_any>.
          IF sy-subrc EQ 0 .
            <fs_any> = gs_konv-kwert.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDIF.

  IF im_struct_name = 'MEREP_OUTTAB_EBANACC'
   AND im_id = 'RM06BKPS'.

    " IF sy-tcode = 'ME5J'.
    DATA:ls_outtab_3 TYPE merep_outtab_ebanacc.
    DATA:l_posid_3 TYPE ps_pspid.
    DATA:l_matnr_3 TYPE matnr.
    CLEAR :gt_zmm024 ,gt_zmm024[], gs_zmm024 .
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_zmm024
      FROM zmm024.
    SORT gt_zmm024 BY posid  matnr .

    LOOP AT ch_outtab ASSIGNING <fs_outtab>.

      CLEAR:ls_outtab_3,l_posid_3,l_matnr_3.

      ASSIGN COMPONENT 'PS_PSP_PNR' OF STRUCTURE <fs_outtab> TO <fs_any>. "WBS号

      "*WBS内码转换WBS元素
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = <fs_any>
        IMPORTING
          output = l_posid_3.
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_outtab> TO <fs_any>. "物料号

      l_matnr_3 = <fs_any> .


      ASSIGN COMPONENT 'BANFN' OF STRUCTURE <fs_outtab> TO <fs_any>.
      gs_ekpo-banfn = <fs_any>.
      ASSIGN COMPONENT 'BNFPO' OF STRUCTURE <fs_outtab> TO <fs_any>.
      gs_ekpo-bnfpo = <fs_any>.

      CLEAR l_longtext1.
      CONCATENATE gs_ekpo-banfn gs_ekpo-bnfpo INTO l_longtext1.

      REFRESH lt_tline.
      CLEAR   ls_tline.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'B01'
          language                = sy-langu
          name                    = l_longtext1
          object                  = 'EBAN'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*   IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = lt_tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      LOOP AT lt_tline INTO ls_tline.
        CONCATENATE  ls_outtab_3-wbscswb ls_tline-tdline INTO ls_outtab_3-wbscswb SEPARATED BY space.
      ENDLOOP.

      ASSIGN COMPONENT 'WBSCSWB' OF STRUCTURE <fs_outtab> TO <fs_any>.
      <fs_any> = ls_outtab_3-wbscswb.
      "根据WBS 号 、物料号读取设计代码
      READ TABLE gt_zmm024 INTO gs_zmm024 WITH KEY posid = l_posid_3 matnr = l_matnr_3 BINARY SEARCH .
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'SJDM' OF STRUCTURE <fs_outtab> TO <fs_any>.
        <fs_any> = gs_zmm024-sjdm.  "设计代码

      ENDIF.


    ENDLOOP.
  ENDIF.

  IF im_struct_name = 'MEREP_OUTTAB_ACCOUNTING'
  AND im_id = 'RM06EKPS'.

    DATA:ls_outtab_4 TYPE merep_outtab_accounting.
    DATA:l_posid_4 TYPE ps_pspid.
    DATA:l_matnr_4 TYPE matnr.
    CLEAR :gt_zmm024 ,gt_zmm024[], gs_zmm024.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_zmm024
      FROM zmm024.
    SORT gt_zmm024 BY posid  matnr .

    LOOP AT ch_outtab ASSIGNING <fs_outtab>.

      CLEAR: ls_outtab_4,l_posid_4,l_matnr_4.

      ASSIGN COMPONENT 'PS_PSP_PNR' OF STRUCTURE <fs_outtab> TO <fs_any>. "WBS号

      "*WBS内码转换WBS元素
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = <fs_any>
        IMPORTING
          output = l_posid_4.
      ASSIGN COMPONENT 'EMATN' OF STRUCTURE <fs_outtab> TO <fs_any>. "物料号

      l_matnr_4 = <fs_any> .

      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_4-ebeln = <fs_any>.


      CLEAR gs_ekko.
      SELECT SINGLE * FROM ekko INTO
      CORRESPONDING FIELDS OF gs_ekko WHERE  ebeln = ls_outtab_4-ebeln .
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'YFCDF' OF STRUCTURE <fs_outtab> TO <fs_any>.  "运费承担方
        <fs_any> = gs_ekko-yfcdf .
        ASSIGN COMPONENT 'FPTG' OF STRUCTURE <fs_outtab> TO <fs_any>.   "发票提供在货款支付前后
        <fs_any> = gs_ekko-fptg .
        ASSIGN COMPONENT 'ZBNX' OF STRUCTURE <fs_outtab> TO <fs_any>.   "乙方免费质保年限
        <fs_any> = gs_ekko-zbnx .
        ASSIGN COMPONENT 'BJZL' OF STRUCTURE <fs_outtab> TO <fs_any>.  "报检资料一式几份
        <fs_any> = gs_ekko-bjzl .
      ENDIF.


      ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_outtab> TO <fs_any>.
      ls_outtab_4-ebelp = <fs_any>.

      CLEAR l_longtext.
      CONCATENATE ls_outtab_4-ebeln ls_outtab_4-ebelp INTO l_longtext.

      REFRESH lt_tline.
      CLEAR ls_tline.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'F05'
          language                = sy-langu
          name                    = l_longtext
          object                  = 'EKPO'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*   IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = lt_tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      LOOP AT lt_tline INTO ls_tline.
        CONCATENATE  ls_outtab_4-wbscswb ls_tline-tdline INTO ls_outtab_4-wbscswb SEPARATED BY space.
      ENDLOOP.

      ASSIGN COMPONENT 'WBSCSWB' OF STRUCTURE <fs_outtab> TO <fs_any>.
      <fs_any> = ls_outtab_4-wbscswb.
      "根据WBS 号 、物料号读取设计代码
      READ TABLE gt_zmm024 INTO gs_zmm024 WITH KEY posid = l_posid_4 matnr = l_matnr_4 BINARY SEARCH .
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'SJDM' OF STRUCTURE <fs_outtab> TO <fs_any>.
        <fs_any> = gs_zmm024-sjdm.  "设计代码

      ENDIF.

    ENDLOOP.
  ENDIF.

  "ME1M 查找物料组 、增加物料组名称、供应商名称 ADD IT02 160107 BEGIN
  IF im_struct_name = 'MEREP_OUTTAB_INFREC'
    AND im_id = 'RM06IM00'.

    DATA:zme1m_matnr TYPE matnr,
         zlifnr      TYPE lifnr.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_t023t
      FROM t023t
      WHERE spras = '1' .
    SORT gt_t023t BY matkl .
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_lfa1
     FROM lfa1 .
    SORT gt_lfa1 BY lifnr.

    LOOP AT ch_outtab ASSIGNING <fs_outtab>.

      CLEAR:gs_mara, zme1m_matnr.
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_outtab> TO <fs_any>. "物料号
      IF sy-subrc = 0.
        zme1m_matnr = <fs_any> .
        SELECT SINGLE * INTO gs_mara
          FROM mara
          WHERE  matnr = zme1m_matnr.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'MATKL' OF STRUCTURE <fs_outtab> TO <fs_any>. "物料组
          IF sy-subrc = 0.
            <fs_any> = gs_mara-matkl.
          ENDIF.
          ASSIGN COMPONENT 'ZWGBEZ' OF STRUCTURE <fs_outtab> TO <fs_any> .   " 物料组名称
          IF sy-subrc = 0.
            READ TABLE gt_t023t INTO gs_t023t WITH KEY matkl = gs_mara-matkl BINARY SEARCH.
            IF sy-subrc = 0.
              <fs_any> = gs_t023t-wgbez.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <fs_outtab> TO <fs_any> . "供应商名称
      IF sy-subrc = 0.
        zlifnr = <fs_any> .
        READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = zlifnr BINARY SEARCH.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'ZGYNAME' OF STRUCTURE <fs_outtab> TO <fs_any> .
          IF sy-subrc = 0 .
            <fs_any> = gs_lfa1-name1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  "ADD IT02 160107 END

  " add ME1P 采购历史记录增加物料组、采购组 IT02 160107 begin

  IF im_struct_name = 'MEREP_OUTTAB_PRHIS'
     AND im_id = 'RM06IBP0'.
    DATA:t_ebeln TYPE ebeln,
         t_ebelp TYPE ebelp,
         "t_matnr TYPE  matnr,
         t_matkl TYPE matkl.
    REFRESH :gt_ekko ,
             gt_ekpo.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_ekko
      FROM ekko.
    SORT gt_ekko BY ebeln.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_ekpo
      FROM ekpo
      FOR ALL ENTRIES IN gt_ekko
      WHERE ebeln = gt_ekko-ebeln.
    SORT gt_ekpo BY ebeln ebelp matnr.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_t023t
        FROM t023t
        WHERE spras = '1' .
    SORT gt_t023t BY matkl .
    LOOP AT ch_outtab ASSIGNING <fs_outtab>.
      CLEAR:t_ebeln ,t_ebelp,t_matnr,t_matkl,
         gs_ekko,gs_ekpo.

      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_outtab> TO <fs_any> . "物料组
      IF sy-subrc = 0.
        t_matnr = <fs_any>.
      ENDIF.
      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_outtab> TO <fs_any> . "采购订单
      IF sy-subrc = 0.
        t_ebeln = <fs_any>.
      ENDIF.
      ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_outtab> TO <fs_any> . "采购订单行号
      IF sy-subrc = 0.
        t_ebelp = <fs_any>.
      ENDIF.
      READ TABLE gt_ekko INTO gs_ekko WITH KEY  ebeln = t_ebeln BINARY SEARCH.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'ZEKGRP' OF STRUCTURE <fs_outtab> TO <fs_any> . "采购组
        IF sy-subrc = 0 .
          <fs_any> = gs_ekko-ekgrp.
        ENDIF.


      ENDIF.
      READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = t_ebeln ebelp = t_ebelp matnr = t_matnr BINARY SEARCH.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'ZMATKL' OF STRUCTURE <fs_outtab> TO <fs_any> . "物料组
        IF sy-subrc = 0 .
          <fs_any> = gs_ekpo-matkl.
          t_matkl =  gs_ekpo-matkl.
        ENDIF.

      ENDIF.
      ASSIGN COMPONENT 'ZWGBEZ' OF STRUCTURE <fs_outtab> TO <fs_any> .   " 物料组名称
      IF sy-subrc = 0.
        READ TABLE gt_t023t INTO gs_t023t WITH KEY matkl = t_matkl BINARY SEARCH.
        IF sy-subrc = 0.
          <fs_any> = gs_t023t-wgbez.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDIF.
  " add ME1P 采购历史记录增加物料组、采购组 IT02 160107 end

  "  *采购申请报表ME5A&ME2N报表增强 by it02 20170810 begin .
  IF im_struct_name = 'MEREP_OUTTAB_EBAN'
    AND im_id = 'RM06BA00'.

*    TYPES:BEGIN OF ty_cgsq,
*            banfn TYPE eban-banfn,  "采购申请号
*            bnfpo TYPE eban-bnfpo,  "采购申请的项目编号
*          END OF ty_cgsq.
*
*    TYPES:BEGIN OF ty_xmh,
*            psphi TYPE prps-psphi,
*          END OF ty_xmh.
*
    TYPES:BEGIN OF ty_wlh,
            matnr TYPE mara-matnr,  "物料号
          END OF ty_wlh.

    DATA:gt_wlh TYPE TABLE OF ty_wlh, "物料号
         gs_wlh TYPE ty_wlh.
*
*    DATA:gt_cgsq TYPE TABLE OF ty_cgsq,  "采购申请
*         gs_cgsq TYPE ty_cgsq.
*
*    DATA:gt_xmh TYPE TABLE OF ty_xmh,  "项目号
*         gs_xmh TYPE ty_xmh.

*    DATA:gt_ebkn TYPE TABLE OF ebkn,  "采购申请账户设置
*         gs_ebkn TYPE ebkn.
*
*    DATA:gt_prps TYPE TABLE OF prps,  "WBS元素
*         gs_prps TYPE prps.
*
*    DATA:gt_proj TYPE TABLE OF proj, "项目名称
*         gs_proj TYPE proj.
*
*    DATA:gt_mara TYPE TABLE OF mara. "物料号
*
*    DATA:t_banfn TYPE ebkn-banfn,
*         t_bnfpo TYPE ebkn-bnfpo.

    "检索出相应的采购申请及行项目号
    MOVE-CORRESPONDING ch_outtab TO gt_cgsq.
    SORT gt_cgsq BY banfn bnfpo.

    "检索出相应不同的物料号
    MOVE-CORRESPONDING ch_outtab TO gt_wlh .
    SORT gt_wlh BY matnr.
    DELETE ADJACENT DUPLICATES FROM gt_wlh COMPARING matnr.

    "根据采购申请号查询采购申请账户设置信息
    IF gt_cgsq IS NOT INITIAL.
      SELECT * INTO TABLE gt_ebkn
       FROM ebkn
       FOR ALL ENTRIES IN gt_cgsq
       WHERE banfn = gt_cgsq-banfn
       AND   bnfpo = gt_cgsq-bnfpo .
      SORT gt_ebkn BY banfn bnfpo.

      IF gt_ebkn IS NOT INITIAL.
        "根据采购申请账户的WBS号查询ＷＢＳ相关信息
        SELECT * INTO TABLE gt_prps
           FROM prps
           FOR ALL ENTRIES IN gt_ebkn
           WHERE pspnr = gt_ebkn-ps_psp_pnr.

        SORT gt_prps BY pspnr.

        "检索出不同的项目号
        MOVE-CORRESPONDING  gt_prps TO gt_xmh.
        SORT gt_xmh BY psphi.
        DELETE ADJACENT DUPLICATES FROM gt_xmh COMPARING psphi.

        "查询项目名称
        IF gt_xmh IS NOT INITIAL.
          SELECT * INTO TABLE gt_proj
            FROM proj
            FOR ALL ENTRIES IN gt_xmh
            WHERE pspnr = gt_xmh-psphi.
          SORT gt_proj BY  pspnr.

        ENDIF.
        "查询物料号
        IF gt_wlh IS NOT INITIAL.
          SELECT * INTO TABLE gt_mara
            FROM mara
            FOR ALL ENTRIES IN gt_wlh
            WHERE matnr = gt_wlh-matnr.
          SORT gt_mara BY matnr.
        ENDIF.
      ENDIF.
    ENDIF.

    "遍历检查出的ALV表查询：WBS号、项目名称、品牌
    LOOP AT ch_outtab ASSIGNING <fs_outtab>.
      CLEAR:ls_outtab_1.
      CLEAR:t_banfn,t_bnfpo,t_matnr.

      "检索出相应的采购申请号
      ASSIGN COMPONENT 'BANFN' OF STRUCTURE <fs_outtab> TO <fs_any> .
      IF sy-subrc EQ 0.
        t_banfn =  <fs_any>.
      ENDIF.

      "检索出相应的采购申请行号
      ASSIGN COMPONENT 'BNFPO' OF STRUCTURE <fs_outtab> TO <fs_any> .
      IF sy-subrc EQ 0.
        t_bnfpo =  <fs_any>.
      ENDIF.

      "检索出相应的物料号
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_outtab> TO <fs_any> .
      IF sy-subrc EQ 0.
        t_matnr =  <fs_any>."物料号
      ENDIF.

      "根据检索出的采购申请、行号去查询WBS号、项目名称
      READ TABLE gt_ebkn INTO gs_ebkn WITH KEY banfn =  t_banfn
                                               bnfpo = t_bnfpo
                                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE gt_prps INTO gs_prps WITH KEY pspnr = gs_ebkn-ps_psp_pnr
                                                 BINARY SEARCH.
        IF sy-subrc EQ 0 .
          "检索wbs号
          ASSIGN COMPONENT 'ZPS_PSP_PNR' OF STRUCTURE <fs_outtab> TO <fs_any> .
          IF sy-subrc EQ 0.
            <fs_any> = gs_prps-posid.
          ENDIF.

          READ TABLE gt_proj INTO gs_proj WITH KEY  pspnr = gs_prps-psphi
                                                    BINARY SEARCH.
          IF sy-subrc EQ 0.
            "检索项目名称
            ASSIGN COMPONENT 'ZPOST1' OF STRUCTURE <fs_outtab> TO <fs_any> .
            IF sy-subrc EQ 0.
              <fs_any> = gs_proj-post1. "项目名称
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      "检索品牌
      READ TABLE gt_mara INTO gs_mara WITH KEY matnr = t_matnr
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 'ZEXTWG' OF STRUCTURE <fs_outtab> TO <fs_any> .
        IF sy-subrc EQ 0.
          <fs_any>  = gs_mara-extwg.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "  *采购申请报表ME5A&ME2N报表增强 by it02 20170810 end.

ENDMETHOD.
ENDCLASS.
