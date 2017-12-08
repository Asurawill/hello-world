*&---------------------------------------------------------------------*
*& 程序名称:ZMM045
*& 作者    :余柏烨
*& 开发日期:
*& 请求号  :
*& 描述    :公司间对账
*& 开发申请：
*& 变更记录
*&
** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*

REPORT ZMM045.

INCLUDE ZMM045_CLS.

INCLUDE ZMM045_TOP.

INCLUDE ZMM045_SEL.

INCLUDE ZMM045_FRM.

INCLUDE ZMM045_PBO.

INCLUDE ZMM045_PAI.

INITIALIZATION .

AT SELECTION-SCREEN.

START-OF-SELECTION .
  CALL SCREEN 9001 .
