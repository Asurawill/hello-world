*&---------------------------------------------------------------------*
*& Report  ZPP008
*&
*&---------------------------------------------------------------------*
*&程序名称： 生产用量信息报表
*&开发日期：2014-02-12
*&创建者：  汪昱
*&
*&---------------------------------------------------------------------*
*&概要说明
*&
*&---------------------------------------------------------------------*
*&变更记录
*&日期       修改者         传输请求号     修改内容及原因
*&----------  ------------   ------------   ----------------------------*
*&2015-10-14  HANDYWLJ        ED1K903120   添加字段
*&---------------------------------------------------------------------*

REPORT ZPP008.

INCLUDE ZPP008_TOP.

START-OF-SELECTION.
  PERFORM FRM_AUTH_CHECK.
  PERFORM FRM_GET_DATA_HEAD." 获取抬头数据
  PERFORM FRM_GET_DATA_ITEM. " 获取组件内表

END-OF-SELECTION.

CALL SCREEN 9001.

INCLUDE ZPP008_PBO.
INCLUDE ZPP008_PAI.
INCLUDE ZPP008_FORMS.
