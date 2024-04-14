*&---------------------------------------------------------------------*
*&  Include           ZVLC_REPORT_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_MAIN_DATA
*&---------------------------------------------------------------------*
FORM get_main_data CHANGING ct_data TYPE tt_data.

  DATA: lt_bseg TYPE TABLE OF bseg,
        ls_bseg TYPE bseg,
        ls_data TYPE ts_data.

  SELECT * FROM bseg INTO TABLE lt_bseg UP TO 100 ROWS.
  IF sy-subrc = 0.
    LOOP AT lt_bseg INTO ls_bseg.
      MOVE-CORRESPONDING: ls_bseg TO ls_data.
      APPEND ls_data TO ct_data.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PRINT_OXML
*&---------------------------------------------------------------------*
FORM print_oxml USING iv_template_id TYPE zoxml_template_id.

  DATA: lo_merger  TYPE REF TO zcl_oxml_merger,
        lo_error   TYPE REF TO zcx_oxml_merger,
        lv_message TYPE string.

  DATA: ls_processing_options  TYPE zoxml_processing_options,
        ls_format_options      TYPE zoxml_format_options,
        lv_compressed_filename TYPE string,
        lv_compress_to_zip     TYPE abap_bool.

  DATA: lt_merge_table TYPE zoxml_merge_table,
        ls_merge_item  TYPE zoxml_merge_item.

  DATA: lt_document      TYPE zoxml_binary_table,
        lv_document_size TYPE i.

  DATA: lv_uuid           TYPE sysuuid_c,
        lv_temp_dir       TYPE string,
        lv_filename(1024) TYPE c.

  " ‘ормируем документ, подставл€€ данные в шаблон из репозитари€
  CREATE OBJECT lo_merger.
  TRY.
      lo_merger->merge(
      EXPORTING
        iv_template_id         = iv_template_id             " идентификатор шаблона в репозитарии
        it_merge_table         = lt_merge_table             " таблица подстановки значений
        is_processing_options  = ls_processing_options      " параметры обработки таблиц шаблона
        is_format_options      = ls_format_options          " параметры форматировани€ значений
        iv_compressed_filename = lv_compressed_filename     " если выбрана опци€ архивации документа - им€ файла в архиве
        iv_compress_to_zip     = abap_false                 " опци€ дл€ архивации документа
      CHANGING
        ct_document            = lt_document                " выходной документ
        cv_document_size       = lv_document_size ).        " размер выходного документа

    CATCH zcx_oxml_merger INTO lo_error.
      lv_message = lo_error->get_text( ).
      MESSAGE lv_message TYPE 'E'.
  ENDTRY.

* ‘ормируем путь файла дл€ сохранени€
  CALL METHOD cl_gui_frontend_services=>get_temp_directory
    CHANGING
      temp_dir             = lv_temp_dir
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc = 0.
    CALL METHOD cl_gui_cfw=>flush.

    "UUID
    CALL FUNCTION 'SYSTEM_UUID_C_CREATE'
      IMPORTING
        uuid = lv_uuid.

    CONCATENATE lv_temp_dir '\' lv_uuid '.xml' INTO lv_filename.
  ENDIF.

* ќткрываем файл
  IF lv_filename IS NOT INITIAL.
    CALL FUNCTION 'ZOXML_SAVE_LOCAL'
      EXPORTING
        iv_document_size = lv_document_size
        iv_extention     = 'XML'
        iv_filetype      = 'BIN'
        iv_execute       = 'X'
        iv_open_with     = 'E'
      CHANGING
        ct_document      = lt_document
        cv_file_name     = lv_filename.
  ENDIF.

ENDFORM.
