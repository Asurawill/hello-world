class ZCL_IM_IDCN_GRIR_GNB definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_IDCN_GRIR_GNB .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_IDCN_GRIR_GNB IMPLEMENTATION.


  METHOD if_idcn_grir_gnb~post_selection_control.
    "add  含税值、含税价、净值、交货单编码 by it02 20161216 begin
    IF ct_content_item IS NOT INITIAL .
      TYPES:BEGIN OF ty_ebeln,
              ebeln TYPE ebeln,
            END OF ty_ebeln .

      TYPES:BEGIN OF ty_mblnr,
              mblnr TYPE mblnr,
              mjahr TYPE mjahr,
            END OF ty_mblnr .

      DATA:gt_mblnr TYPE TABLE OF ty_mblnr,
           gs_mblnr TYPE ty_mblnr.

      DATA:gt_ekko TYPE TABLE OF ekko,
           gs_ekko TYPE ekko.

      DATA:gt_konv TYPE TABLE OF konv,
           gs_konv TYPE konv.

      DATA:gt_ebeln TYPE TABLE OF ty_ebeln,
           gs_ebeln TYPE ty_ebeln.

      DATA:gt_mkpf TYPE TABLE OF mkpf,
           gs_mkpf TYPE mkpf.

      FIELD-SYMBOLS:<fs_data> TYPE LINE OF  idcn_t_grir_gnb_item.

      "采购订单号分类
      REFRESH:gt_ebeln .
      MOVE-CORRESPONDING  ct_content_item TO gt_ebeln.
      SORT gt_ebeln BY ebeln .
      DELETE ADJACENT DUPLICATES FROM gt_ebeln COMPARING ebeln .

      "物料凭证号分类
      MOVE-CORRESPONDING  ct_content_item TO gt_mblnr.
      SORT gt_mblnr BY mblnr mjahr.
      DELETE ADJACENT DUPLICATES FROM gt_mblnr COMPARING mblnr mjahr .

      "查询物料凭证抬头信息
      IF gt_mblnr IS NOT INITIAL.
        SELECT * INTO TABLE gt_mkpf
          FROM mkpf
          FOR ALL ENTRIES IN gt_mblnr
          WHERE mblnr = gt_mblnr-mblnr
           AND  mjahr = gt_mblnr-mjahr.
        SORT gt_mkpf BY mblnr mjahr.
      ENDIF.

      "查询采购订单抬头信息
      IF gt_ebeln IS NOT INITIAL.
        SELECT * INTO TABLE gt_ekko
        FROM ekko
        FOR ALL ENTRIES IN gt_ebeln
        WHERE ebeln = gt_ebeln-ebeln .
        SORT gt_ekko BY ebeln .
      ENDIF.

     "采购采购订单条件信息
     if gt_ekko is not initial.
         SELECT * INTO TABLE gt_konv
        FROM konv
        FOR ALL ENTRIES IN gt_ekko
        WHERE knumv = gt_ekko-knumv
        AND   stunr  EQ '001' .
      SORT gt_konv BY knumv kposn.
      endif.

      LOOP AT ct_content_item  ASSIGNING <fs_data>.
            READ TABLE gt_ekko INTO gs_ekko WITH KEY ebeln = <fs_data>-ebeln
                                            BINARY SEARCH.
           IF sy-subrc EQ 0 .

                READ TABLE gt_konv INTO gs_konv WITH KEY knumv = gs_ekko-knumv
                                                         kposn = <fs_data>-ebelp
                                                BINARY SEARCH.
               IF sy-subrc EQ 0 .
                "含税值
                <fs_data>-zhsz = gs_konv-kbetr  * <fs_data>-menge / gs_konv-kpein.
                 "含税价
                <fs_data>-zhsjg =  <fs_data>-zhsz /  <fs_data>-menge .
               ENDIF.
          ENDIF.
        "净值
        <fs_data>-zjz = <fs_data>-wrbtr / <fs_data>-menge .

        "交货单编码 。
        READ TABLE gt_mkpf INTO gs_mkpf WITH KEY mblnr = <fs_data>-mblnr
                                                 mjahr = <fs_data>-mjahr
                                        BINARY SEARCH .
        IF sy-subrc EQ 0 .
          <fs_data>-zjhdbm = gs_mkpf-xblnr.
        ENDIF.
      ENDLOOP.

      "add  含税值、含税价、净值、交货单编码 by it02 20161216 end
      "   BREAK-POINT.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
