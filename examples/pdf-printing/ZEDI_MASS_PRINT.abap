FUNCTION zedi_mass_print.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  TABLES
*"      CT_CARD_DATA STRUCTURE  ZDF_CARD_030003
*"----------------------------------------------------------------------

  DATA: ls_card_data      TYPE zdf_card_030003,
        lt_card_data      TYPE TABLE OF zdf_card_030003,
        ls_card_data_pkg  TYPE zdf_card_030003,
        lt_card_data_pkg  TYPE TABLE OF zdf_card_030003,
        ls_cardfiles      TYPE /dfs/rcrd_record_data,
        lt_cardfiles      TYPE TABLE OF /dfs/rcrd_record_data,
        ls_output_params  TYPE sfpoutputparams,
        ls_add_params_reg TYPE zedi_030003_add_params_reg,
        lt_gln            TYPE TABLE OF zdf_card_030003-sh_to_gln WITH HEADER LINE,
        lv_pdf            TYPE fpcontent,
        lt_messages       TYPE esp1_message_tab_type,
        ls_messages       TYPE esp1_message_wa_type,
        lv_error.

* Получим список GLN пунктов разгрузки
  LOOP AT ct_card_data INTO ls_card_data.
    APPEND ls_card_data-sh_to_gln TO lt_gln.
  ENDLOOP.
  SORT lt_gln.
  DELETE ADJACENT DUPLICATES FROM lt_gln.

* Вызов экрана для ввода дополнительных параметров реестра
  ls_add_params_reg-copies_reg = 2.
  ls_add_params_reg-copies_doc = 2.
  PERFORM get_add_parameters_reg CHANGING ls_add_params_reg
                                          lv_error.
  IF lv_error IS NOT INITIAL.
    EXIT.
  ENDIF.

* Параметры печати
  ls_output_params-nodialog = abap_true.
  ls_output_params-getpdf   = abap_true.

* Очистим старые данные печати
  PERFORM clear_old_print_data.

* Запускаем приложение
  CALL METHOD cl_gui_frontend_services=>execute
    EXPORTING
      application            = 'AcroRd32'
    EXCEPTIONS
      cntl_error             = 1
      error_no_gui           = 2
      bad_parameter          = 3
      file_not_found         = 4
      path_not_found         = 5
      file_extension_unknown = 6
      error_execute_failed   = 7
      synchronous_failed     = 8
      not_supported_by_gui   = 9
      OTHERS                 = 10.

* Ждем запуска приложения
  WAIT UP TO 5 SECONDS.

* Формируем очередь печати
  LOOP AT lt_gln.
    CLEAR: lv_pdf, ls_messages, lv_error.
    REFRESH: lt_card_data_pkg.

    LOOP AT ct_card_data INTO ls_card_data WHERE sh_to_gln = lt_gln.
      APPEND ls_card_data TO lt_card_data_pkg.
    ENDLOOP.

*   Формирование и печать реестра
    CALL FUNCTION 'ZEDI_PRINT_REGISTRY'
      EXPORTING
        is_output_params = ls_output_params
      IMPORTING
        ev_pdf           = lv_pdf
      TABLES
        ct_card_data     = lt_card_data_pkg
      CHANGING
        cs_add_params    = ls_add_params_reg.

    PERFORM print_file_from_xstr USING lv_pdf
                                       ls_add_params_reg-copies_reg
                              CHANGING lv_error.

    IF lv_error IS NOT INITIAL.
      CASE lv_error.
        WHEN '1'.
          "&1 &2 - ошибка данных файла печати!
          macros_set_status_msg ls_messages 'Z030_EDI' 'E' '048' 'Реестр для GLN' lt_gln.
        WHEN '2'.
          "&1 &2 - ошибка записи файла на диск!
          macros_set_status_msg ls_messages 'Z030_EDI' 'E' '049' 'Реестр для GLN' lt_gln.
        WHEN '3'.
          "&1 &2 - ошибка печати!
          macros_set_status_msg ls_messages 'Z030_EDI' 'E' '047' 'Реестр для GLN' lt_gln.
        WHEN OTHERS.
          "&1 &2 - ошибка печати!
          macros_set_status_msg ls_messages 'Z030_EDI' 'E' '047' 'Реестр для GLN' lt_gln.
      ENDCASE.
      APPEND ls_messages TO lt_messages.
    ELSE.
      " &1 &2 - успешно!
      macros_set_status_msg ls_messages 'Z030_EDI' 'S' '046' 'Реестр для GLN' lt_gln.
      APPEND ls_messages TO lt_messages.
    ENDIF.

*   Формирование и печать накладных
    LOOP AT lt_card_data_pkg INTO ls_card_data_pkg.
      CLEAR: lv_pdf, ls_messages, lv_error.
      REFRESH: lt_card_data.

      APPEND ls_card_data_pkg TO lt_card_data.

      CALL FUNCTION 'ZEDI_PRINT_DELIVERY'
        EXPORTING
          is_output_params = ls_output_params
        IMPORTING
          ev_pdf           = lv_pdf
        TABLES
          ct_card_data     = lt_card_data.

      PERFORM print_file_from_xstr USING lv_pdf
                                         ls_add_params_reg-copies_doc
                                CHANGING lv_error.

      IF lv_error IS NOT INITIAL.
        CASE lv_error.
          WHEN '1'.
            "&1 &2 - ошибка данных файла печати!
            macros_set_status_msg ls_messages 'Z030_EDI' 'E' '048' 'Накладная' ls_card_data_pkg-delivery_id.
          WHEN '2'.
            "&1 &2 - ошибка записи файла на диск!
            macros_set_status_msg ls_messages 'Z030_EDI' 'E' '049' 'Накладная' ls_card_data_pkg-delivery_id.
          WHEN '3'.
            " &1 &2 - ошибка печати!
            macros_set_status_msg ls_messages 'Z030_EDI' 'E' '047' 'Накладная' ls_card_data_pkg-delivery_id.
          WHEN OTHERS.
            " &1 &2 - ошибка печати!
            macros_set_status_msg ls_messages 'Z030_EDI' 'E' '047' 'Накладная' ls_card_data_pkg-delivery_id.
        ENDCASE.
        APPEND ls_messages TO lt_messages.
      ELSE.
        " &1 &2 - успешно!
        macros_set_status_msg ls_messages 'Z030_EDI' 'S' '046' 'Накладная' ls_card_data_pkg-delivery_id.
        APPEND ls_messages TO lt_messages.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

* Статус печати
  IF lt_messages IS NOT INITIAL.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_messages.
  ENDIF.

ENDFUNCTION.
