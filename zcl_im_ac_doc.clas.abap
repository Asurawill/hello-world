class ZCL_IM_AC_DOC definition
  public
  final
  create public .

public section.

  interfaces IF_EX_AC_DOCUMENT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_AC_DOC IMPLEMENTATION.


  method if_ex_ac_document~change_after_check.
    ex_document-header-bktxt = im_document-header-bktxt.
  endmethod.


  METHOD if_ex_ac_document~change_initial.
    ex_document-header-bktxt = im_document-header-bktxt.

    DATA:it_accit TYPE accit_t.
    DATA:wa_accit TYPE accit.
    DATA:it_sub_t TYPE accit_sub_t.
    DATA:wa_accit_sub TYPE accit_sub.
    DATA:lt_vbak TYPE TABLE OF vbak.
    DATA:lt_vbap TYPE TABLE OF vbap.
    DATA:lt_cska TYPE TABLE OF cska.
    DATA:lw_vbap LIKE LINE OF lt_vbap.
    DATA:lw_vbak LIKE LINE OF lt_vbak.
    DATA:lw_cska LIKE LINE OF lt_cska.
    DATA l_flag TYPE c.
    DATA:POSNRTMP TYPE VBAP-POSNR.
     DATA:lt2_vbap TYPE TABLE OF vbap.
     DATA:lw2_vbap LIKE LINE OF lt_vbap.
    "migo 231/231e 232/232/e 过账检查会计凭证 获取 移动类型和特殊库存标识 进行判断是否满足替代要求 进入替代程序ZRGGBS00 进行检查替代
    IF sy-tcode = 'MIGO' OR sy-tcode = 'MB1A'.
      CLEAR l_flag.
      LOOP AT im_document-item INTO wa_accit.
        IF ( wa_accit-bwart = '231' OR wa_accit-bwart = '232' ) AND ( wa_accit-sobkz = 'E' OR wa_accit-sobkz = '').
          l_flag = 'X'.
          IF wa_accit-sobkz = 'E'.
          l_flag = 'E'. "如果特殊库存标示为E 需要在替代结算销售订单行时通过四代增强tcode:se20 ZAC2312E_SUBSTITUTION 特殊处理
          ENDIF.
        ENDIF.
      ENDLOOP.
      SET PARAMETER ID '2312E_FLAG' FIELD l_flag.
    ENDIF.



    IF sy-tcode CP 'VL0*' .

      "----交货单交货按销售订单虚拟物料行进行会计凭证结算
      SELECT vbeln
             auart
        FROM vbak
        INTO CORRESPONDING FIELDS OF TABLE lt_vbak
        FOR ALL ENTRIES IN im_document-item
        WHERE vbeln = im_document-item-vbel2
        AND (   auart = 'ZPO'
             OR auart = 'ZZG'
             OR auart = 'ZF1'
             OR auart = 'ZWV'
             OR auart = 'ZSO'
              )
        .
      IF lt_vbak[] IS NOT INITIAL.

        SELECT
              ktopl
              kstar
          FROM cska
          INTO CORRESPONDING FIELDS OF TABLE lt_cska
          FOR ALL ENTRIES IN im_document-item
          WHERE ktopl = '1000'
          AND   kstar = im_document-item-hkont
          .
*        SELECT vbeln
*                posnr
*                pstyv
*                uepos
*           FROM vbap
*           INTO CORRESPONDING FIELDS OF TABLE lt_vbap
*           FOR ALL ENTRIES IN im_document-item
*           WHERE vbeln = im_document-item-mat_kdauf
*           AND   posnr = im_document-item-mat_kdpos
*           .
         SELECT  vbeln
                posnr
                pstyv
                uepos
           FROM vbap
           INTO CORRESPONDING FIELDS OF TABLE lt2_vbap
           FOR ALL ENTRIES IN lt_vbak
           WHERE vbeln = lt_vbak-vbeln .
        SORT lt2_vbap BY VBELN POSNR.
        SELECT  vbeln
                posnr
                pstyv
                uepos
           FROM vbap
           INTO CORRESPONDING FIELDS OF TABLE lt_vbap
           FOR ALL ENTRIES IN lt_vbak
           WHERE vbeln = lt_vbak-vbeln
           AND   pstyv IN ( 'Z03',
                            'Z04',
                            'Z05',
                            'Z06',
                            'Z07',
                            'Z08',
                            'Z09',
                            'Z33',
                            'Z34',
                            'Z43',
                            'Z44',
                            'Z45',
                            'Z46',
                            'Z47',
                            'Z48' ) .
         "IT02 INSERT BEGIN
           SORT LT_VBAP BY VBELN POSNR  .

          "IT02 INSERT BEGIN

       " 150520 之前的处理更换换行项目逻辑begin，HANDLJ
*        LOOP AT im_document-item INTO wa_accit.
*          CLEAR lw_cska.
*          READ TABLE lt_cska INTO lw_cska WITH KEY kstar = wa_accit-hkont.
*          IF sy-subrc = 0.
*            CLEAR lw_vbap.
*            READ TABLE lt_vbap INTO lw_vbap WITH KEY vbeln = wa_accit-vbel2
*                                                     posnr = wa_accit-posn2.
*            IF sy-subrc = 0.
*              CLEAR lw_vbak.
*              MOVE-CORRESPONDING wa_accit TO wa_accit_sub.
*              wa_accit_sub-kdauf = lw_vbap-vbeln.
*
*              READ TABLE lt_vbak INTO lw_vbak WITH KEY vbeln = lw_vbap-vbeln.
*              IF sy-subrc = 0.
**                wa_accit_sub-kdpos     = lw_vbap-uepos. 原来归结到上层行项目
*                IF lw_vbak-auart = 'ZSO'.
*                  wa_accit_sub-kdpos = '000010'. "新修改为归结到订单首行
*                ELSE.
*                  wa_accit_sub-kdpos = '000100'. "新修改为归结到订单首行
*                ENDIF.
*              ENDIF.
*              APPEND wa_accit_sub  TO ex_document-item.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
 "  " 150520 之前的处理更换换行项目逻辑end，HANDLJ
           "150527 之前处理行项目逻辑150527 BEGIN

           SORT LT_VBAP BY VBELN POSNR .

           LOOP AT im_document-item INTO wa_accit.
          CLEAR lw_cska.
          READ TABLE lt_cska INTO lw_cska WITH KEY kstar = wa_accit-hkont.
          IF sy-subrc = 0.
            CLEAR lw_vbap.
            READ TABLE lt_vbap INTO lw_vbap WITH KEY vbeln = wa_accit-vbel2
                                                     posnr = wa_accit-posn2.
            IF sy-subrc = 0.
              CLEAR lw_vbak.
              MOVE-CORRESPONDING wa_accit TO wa_accit_sub.
              wa_accit_sub-kdauf = lw_vbap-vbeln.

              READ TABLE lt_vbak INTO lw_vbak WITH KEY vbeln = lw_vbap-vbeln.
              IF sy-subrc = 0.
*                wa_accit_sub-kdpos     = lw_vbap-uepos. 原来归结到上层行项目
                IF lw_vbak-auart = 'ZSO'.
                LOOP AT lt2_vbap INTO lw2_vbap where vbeln = lw_vbak-vbeln  .
                 CASE LW2_VBAP-POSNR.
               WHEN '000010'.
                 POSNRTMP = '000010' .
                 EXIT.
               WHEN '000020'.
                 POSNRTMP = '000020' .
                       EXIT.
                  WHEN '000030'.
                 POSNRTMP = '000030' .
                       EXIT.
                  WHEN '000040'.
                 POSNRTMP = '000040' .
                       EXIT.
                  WHEN '000050'.
                 POSNRTMP = '000050' .
                       EXIT.
                  WHEN '000060'.
                 POSNRTMP = '000060' .
                       EXIT.
                  WHEN '000070'.
                 POSNRTMP = '000070' .
                      EXIT.
                  WHEN '000080'.
                 POSNRTMP = '000080' .
                  EXIT.
                  WHEN '000090'.
                 POSNRTMP = '000090' .
                 EXIT.
                  WHEN '000100'.
                 POSNRTMP = '000100' .
                    EXIT.
               ENDCASE.
            ENDLOOP.
                  wa_accit_sub-kdpos = POSNRTMP. "新修改为归结到首先替换到100 或200 或300
                ELSE.
               LOOP AT lt2_vbap INTO lw2_vbap where vbeln = lw_vbak-vbeln  .
               CASE LW2_VBAP-POSNR.
               WHEN '000100'.
                 POSNRTMP = '000100' .
                 EXIT.
               WHEN '000200'.
                 POSNRTMP = '000200' .
                    EXIT.
                  WHEN '000300'.
                 POSNRTMP = '000300' .
                     EXIT.
                  WHEN '000400'.
                 POSNRTMP = '000400' .
                       EXIT.
                  WHEN '000500'.
                 POSNRTMP = '000500' .
                     EXIT.
                  WHEN '000600'.
                 POSNRTMP = '000600' .
                     EXIT.
                  WHEN '000700'.
                 POSNRTMP = '000700' .
                     EXIT.
                  WHEN '000800'.
                 POSNRTMP = '000800' .
                     EXIT.
                  WHEN '000900'.
                 POSNRTMP = '000900' .
                    EXIT.
               ENDCASE.
               ENDLOOP.
               wa_accit_sub-kdpos = POSNRTMP. "新修改为归结到首先替换到100 或200 或300
                ENDIF.
              ENDIF.
              APPEND wa_accit_sub  TO ex_document-item.
            ENDIF.
          ENDIF.
        ENDLOOP.
             "150527 之前处理行项目逻辑150527 END


*         "150520 修改逻辑 IT02 BEGIN
*         LOOP AT im_document-item INTO wa_accit.
*          CLEAR lw_cska.
*          READ TABLE lt_cska INTO lw_cska WITH KEY kstar = wa_accit-hkont.
*          IF sy-subrc = 0.
*            CLEAR lw_vbap.
*            "READ TABLE lt_vbap INTO lw_vbap WITH KEY vbeln = wa_accit-vbel2
*            READ TABLE lt_vbap INTO lw_vbap WITH KEY vbeln = wa_accit-vbel2
*                                                     posnr = wa_accit-posn2.
*            IF sy-subrc = 0.
*              CLEAR lw_vbak.
*              MOVE-CORRESPONDING wa_accit TO wa_accit_sub.
*              wa_accit_sub-kdauf = lw_vbap-vbeln.
*
*              READ TABLE lt_vbak INTO lw_vbak WITH KEY vbeln = lw_vbap-vbeln.
*              IF sy-subrc = 0.
**                wa_accit_sub-kdpos     = lw_vbap-uepos. 原来归结到上层行项目
*                IF lw_vbak-auart = 'ZSO'.
*                 " wa_accit_sub-kdpos = '000010'. "新修改为归结到订单首行
*                  wa_accit_sub-kdpos = lw_vbap-uepos.  "新修改为归结到订单首行
*                ELSE.
*                  "wa_accit_sub-kdpos = '000100'. "新修改为归结到订单首行
*                  wa_accit_sub-kdpos = lw_vbap-uepos. "新修改为归结到订单首行
*                ENDIF.
*              ENDIF.
*              APPEND wa_accit_sub  TO ex_document-item.
*            ENDIF.
*
*          ENDIF.
*        ENDLOOP.
*
*        " 150520 修改逻辑 IT02 END
      ENDIF.


    ENDIF.
  ENDMETHOD.


  method IF_EX_AC_DOCUMENT~IS_ACCTIT_RELEVANT.
  endmethod.


  method IF_EX_AC_DOCUMENT~IS_COMPRESSION_REQUIRED.
  endmethod.


  method IF_EX_AC_DOCUMENT~IS_SUPPRESSED_ACCT.
  endmethod.
ENDCLASS.
