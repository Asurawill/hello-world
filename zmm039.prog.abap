*&---------------------------------------------------------------------*
*& Report ZMM039
*&---------------------------------------------------------------------*
*&*& 作者           : HANDYBY
*& 开发日期       : 2017-5-17
*& 申请者         :
*& 描述           ：采购信息记录批导
*& 变更记录
*& ** 修改日期 开发人员  请求号 描述
*&---------------------------------------------------------------------*
REPORT ZMM039.
INCLUDE ZMM039_TOP.
INCLUDE ZMM039_SEL.
INCLUDE ZMM039_FRM.

INITIALIZATION.
  FUNCTXT-ICON_ID   = ICON_EXPORT.
  FUNCTXT-QUICKINFO = '模版下载'(001).
  FUNCTXT-ICON_TEXT = '模版下载'(001).
  SSCRFIELDS-FUNCTXT_01 = FUNCTXT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE .
*&------------选择路径
  PERFORM SELECT_PATH.

AT SELECTION-SCREEN.
*&--------------模板下载
  IF SSCRFIELDS-UCOMM = 'FC01'.
    PERFORM TEMP_EXCEL_GET USING 'ZMM039.XLS'.
  ELSEIF SSCRFIELDS-UCOMM = 'ONLI'.
*&--------------执行上传EXCEL
    IF P_FILE IS  NOT INITIAL.
      IF P_NEW = 'X'.
*&--------------数据导入GT_NEW
        PERFORM FRM_XLS_TO_SAP TABLES GT_NEW.
*&--------------数据修改(物料加前导零),(单位转换)
        PERFORM FRM_DATA_COMFORM .
*&-------------  导入的数据展示给用户
        CLEAR FIELDCATALOG[].
        PERFORM BUILD_FIELDCATALOG_BEFORE.
        PERFORM BUILD_LAYOUT.
        PERFORM DISPLAY_ALV_REPORT_BEFORE.
*&--------------录屏解决保存数据 SE11
*        PERFORM frm_save_data.
**&----------------ALV展示结果
*        PERFORM build_fieldcatalog.
*        PERFORM build_layout.
*        PERFORM display_alv_report.
*      ELSEIF p_modify = 'X'.
**&--------------数据导入
*        PERFORM frm_xls_to_sap TABLES gt_modify.
**&--------------数据修改(物料加前导零),(单位转换)
*        PERFORM frm_data_comform .
**&-----------修改条件信息
*        PERFORM frm_modify_condition.
      ENDIF.
    ENDIF.
  ENDIF.
