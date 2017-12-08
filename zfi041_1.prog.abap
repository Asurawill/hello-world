*&---------------------------------------------------------------------*
*& 程序名称:ZFI041
*& 作者    :余柏烨
*& 开发日期:
*& 请求号  :
*& 描述    :租赁固定资产折旧分摊平台
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*

REPORT ZFI041_1.

INCLUDE ZFI041_1_TOP.
*INCLUDE ZFI041_TOP .

INCLUDE ZFI041_1_SEL.
*INCLUDE ZFI041_SEL .

INCLUDE ZFI041_1_FRM.
*INCLUDE ZFI041_FRM .

INCLUDE ZFI041_1_PBO.
*INCLUDE ZFI041_PBO .

INCLUDE ZFI041_1_PAI.
*INCLUDE ZFI041_PAI .

INITIALIZATION .

AT SELECTION-SCREEN.

START-OF-SELECTION .
  IF P1 = 'X' .
    CALL SCREEN 9001 .
  ELSEIF P2 = 'X' .
    CALL SCREEN 9002 .
  ENDIF.
*  PERFORM FRM_GET_DEAL_DATA .
