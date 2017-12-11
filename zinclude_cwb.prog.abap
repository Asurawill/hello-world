*&---------------------------------------------------------------------*
*& Create by     : it02
*& Create date   : 20170207
*& Request       : ED1K905233
*& Descriptions  : 长文本数据相关代码
*&
*& Modify by     :
*& Modify date   :
*& Request       :
*& Descriptions  :
*&


 TYPES:BEGIN OF textline,
         line(255) TYPE c,
       END OF textline.


 DATA: it_lines TYPE TABLE OF tline,
       wa_lines TYPE tline.

 DATA:texttable TYPE TABLE OF textline .  " tt_text .  "定义一个长文本内表对象
 FORM readtext USING p_editor TYPE REF TO cl_gui_textedit
       VALUE(p_thead) TYPE thead .





   REFRESH it_lines.
   CALL FUNCTION 'READ_TEXT'
     EXPORTING
       client                  = sy-mandt
       id                      = p_thead-tdid
       language                = sy-langu
       name                    = p_thead-tdname
       object                  = p_thead-tdobject
     TABLES
       lines                   = it_lines
     EXCEPTIONS
       id                      = 1
       language                = 2
       name                    = 3
       not_found               = 4
       object                  = 5
       reference_check         = 6
       wrong_access_to_archive = 7
       OTHERS                  = 8.

   IF sy-subrc <> 0 .

     MESSAGE '无此文本!' TYPE 'I'.
   ENDIF.

   "将所读取的长文本内表数据按实际行转化到内表

   CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
     EXPORTING
       language    = sy-langu
     TABLES
       itf_text    = it_lines
       text_stream = texttable.

   "将内表数据赋值给文本编辑器

   CALL METHOD p_editor->set_text_as_stream
     EXPORTING
       text            = texttable
     EXCEPTIONS
       error_dp        = 1
       error_dp_create = 2
       OTHERS          = 3.

 ENDFORM.

 "savetext

 FORM savetext USING p_editor TYPE REF TO cl_gui_textedit
                     VALUE(p_thead) TYPE thead.

   " data texttable type table of textline .

   "从文本编辑器中获取数据给一个内表，数据为长文本格式，未进行分析

   CALL METHOD p_editor->get_text_as_stream
     IMPORTING
       text                   = texttable
     EXCEPTIONS
       error_dp               = 1
       error_cntl_call_method = 2
       OTHERS                 = 3.

   "将内表中的长文本数据转换为标准格式并存放到指定内表中
   CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'  "将内表数据转换到标准行格式
     EXPORTING
       language    = sy-langu
     TABLES
       text_stream = texttable
       itf_text    = it_lines.

   "将内表数据通过函数转换为LRAW类型存放在指定的物理表中

   CALL FUNCTION 'SAVE_TEXT'
     EXPORTING
       header          = p_thead
       savemode_direct = 'X'
     TABLES
       lines           = it_lines
     EXCEPTIONS
       id              = 1
       langue          = 2
       name            = 3
       object          = 4
       OTHERS          = 5.
   IF sy-subrc EQ 0 .
     MESSAGE s000(oo) WITH 'Success!' .
     COMMIT WORK.
   ELSE.
     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .

   ENDIF.

 ENDFORM .
