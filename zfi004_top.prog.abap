*&---------------------------------------------------------------------*
*&  包含                ZFI004_TOP
*&---------------------------------------------------------------------*

************************************************************************
* Tables
************************************************************************
TABLES: T001,PROJ,BKPF .

************************************************************************
* Type Declaration
************************************************************************
TYPES: BEGIN OF TY_ALV ,
         SEL       TYPE C,
         NUM       TYPE I,
         " 项目基本信息
         PSPID     TYPE PROJ-PSPID,
         POST      TYPE PROJ-POST1,
         POSID     TYPE PRPS-POSID,
         POST1     TYPE PRPS-POST1,
         STUFE     TYPE PRPS-STUFE,
         PRART     TYPE PRPS-PRART,
         ZHTLX     TYPE PRPS-ZHTLX,
         ZKHBM     TYPE PRPS-ZKHBM,
         NAME1     TYPE KNA1-NAME1,
         ZHTJR     TYPE PRPS-ZHTJR,
         ZJGJSJE   TYPE PROJ-ZJGJSJE,
         " 项目预算信息
         YSSR      TYPE COEPB-WOGBTR,
         YSCB      TYPE COEPB-WOGBTR,
         YSMLL     TYPE COEPB-WOGBTR,
         YSFY      TYPE COEPB-WOGBTR,
         " 项目实际财务信息
         SJSR      TYPE BSEG-DMBTR,
         SJCB      TYPE BSEG-DMBTR,
         SJMLL     TYPE BSEG-DMBTR,
         WGBFB     TYPE BSEG-DMBTR,
         GCJS      TYPE BSEG-DMBTR,
         GCML      TYPE BSEG-DMBTR,
         SJFY      TYPE BSEG-DMBTR,
         GCSG      TYPE BSEG-DMBTR,
         GCCB_FCSP TYPE BSEG-DMBTR,
         YSZK      TYPE BSEG-DMBTR,
         YFZK      TYPE BSEG-DMBTR,
         YFZG      TYPE BSEG-DMBTR,
         Z8008     TYPE BSEG-DMBTR,
         YKJSFPJE  TYPE BSEG-DMBTR,
         WKJSFPJE  TYPE BSEG-DMBTR,
         " 其它信息
         LJSK      TYPE ZFI005-HSL,
         DQJDYS    TYPE PRPS-ZYSJE1,
         CGE_HS    TYPE EKPO-BRTWR,
         LWHTJE    TYPE EKPO-BRTWR,
         CLCG      TYPE EKPO-BRTWR,
         FBCG      TYPE EKPO-BRTWR,
         YFJE      TYPE ZFI017-ZSQFKJE,
         WSKJE     TYPE PRPS-ZHTJR,
         " 摘要
         TXT       TYPE STRING,
         " 行颜色
         CLR(4)    TYPE C,
       END OF TY_ALV .

************************************************************************
* Internal Table & WorkArea
************************************************************************
DATA: GT_ALV TYPE TABLE OF TY_ALV,
      GS_ALV TYPE TY_ALV.

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
DATA:GS_LAYOUT TYPE LVC_S_LAYO,
     GS_LVC    TYPE LVC_S_FCAT,
     GT_LVC    TYPE LVC_T_FCAT.

************************************************************************
*      DEFINITION
************************************************************************
DEFINE INIT_FIELDCAT.      "  ALV Fieldcat Setting
  GS_LVC-fieldname = &1.
  GS_LVC-coltext   = &2.
  GS_LVC-scrtext_l = &2.
  GS_LVC-scrtext_m = &2.
  GS_LVC-scrtext_s = &2.
  GS_LVC-reptext   = &2.
  GS_LVC-outputlen = &3.
  IF &4 = 'X'.
    GS_LVC-key = 'X'.
  ENDIF.
  GS_LVC-checkbox = &5.
  GS_LVC-edit = &6.
*  GW_LVC-FIX_COLUMN =  &7.
  GS_LVC-hotspot   = &7.
  GS_LVC-ref_field = &9.
  GS_LVC-ref_table = &8.

*IF gw_lvc-fieldname = 'PROJK'.
*   gw_lvc-NO_ZERO = 'X'.
*ENDIF.
  APPEND GS_LVC TO gt_lvc.
  CLEAR GS_LVC.
END-OF-DEFINITION.
