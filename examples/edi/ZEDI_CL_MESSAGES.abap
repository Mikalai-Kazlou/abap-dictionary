class ZEDI_CL_MESSAGES definition
  public
  create public .

*"* public components of class ZEDI_CL_MESSAGES
*"* do not include other source files here!!!
public section.

  data MESSAGE_TYPE type ZEDI_MSG_TYPE read-only .
  data PROXY_DATA_TYPE type TYPENAME read-only .

  methods CONSTRUCTOR
    importing
      !IV_MSG_TYPE type ZEDI_MSG_TYPE optional
    exceptions
      INVALID_MESSAGE_TYPE .
  methods GET_OUTPUT_MESSAGE
    importing
      !IS_MSG_DATA type ANY
    exporting
      !EV_XML_DATA type XSTRING
    exceptions
      PROXY_FAULT
      TRANSFORMATION_ERROR
      CRITICAL_ERROR .
  methods GET_INPUT_MESSAGE
    importing
      !IV_XML_DATA type XSTRING
    exporting
      !ES_MSG_DATA type ANY
    exceptions
      PROXY_FAULT
      TRANSFORMATION_ERROR
      CRITICAL_ERROR .
  methods SIGN_MESSAGE
    importing
      !IS_MSG_DATA type ANY
      !IV_XML_DATA type XSTRING optional
    changing
      !CV_FILE_NAME type STRING optional
      !CV_FILE_PATH type STRING optional
    exceptions
      CONNECTION_PARAMETERS_ERROR
      DOWNLOAD_FILE_ERROR
      SIGN_FILE_ERROR
      CREATE_MESSAGE_ERROR
      CHECK_CERTIFICATE_ERROR .
  methods SEND_MESSAGE
    importing
      !IV_FILE_NAME type STRING
      !IV_FILE_PATH type STRING
    exceptions
      CONNECTION_PARAMETERS_ERROR
      SEND_MESSAGE_ERROR .
  methods CHECK_CERTIFICATE
    importing
      !IV_FILE_PATH type STRING
    exceptions
      INVALID_FILE_PATH
      UPLOAD_ERROR
      DATA_NOT_BOUND
      INVALID_CERTIFICATE .
  class-methods DOWNLOAD_MSG_TO_FILE
    importing
      !IV_DOWNLOAD_TYPE type STRING default ZEDI_IF_FILE_LOAD_TYPE=>SCREEN
      !IV_FILE_PATH type STRING optional
      !IV_FILE_DATA type XSTRING
    exceptions
      INVALID_FILE_PATH
      DOWNLOAD_ERROR
      DOWNLOAD_TYPE_NOT_SUPPORTED .
  class-methods UPLOAD_MSG_FROM_FILE
    importing
      !IV_UPLOAD_TYPE type STRING default ZEDI_IF_FILE_LOAD_TYPE=>FRONTEND
      !IV_FILE_PATH type STRING
    exporting
      !EV_FILE_DATA type XSTRING
    exceptions
      INVALID_FILE_PATH
      UPLOAD_ERROR
      UPLOAD_TYPE_NOT_SUPPORTED .
  class-methods CER_IMPORT
    exceptions
      SETTINGS_NOT_FOUND
      CERTIFICATE_IMPORT_ERROR .
  class-methods CRL_IMPORT
    exceptions
      SETTINGS_NOT_FOUND
      CERTIFICATE_IMPORT_ERROR .
  class-methods GET_MSG_ID_PARAMETERS
    exporting
      !EV_MSG_DATE_TIME type C
      !EV_MSG_ID type C
      !EV_DOC_ID type C .
  class-methods GET_CARD_OBJTYPE
    importing
      !IV_MSG_TYPE type ZEDI_MSG_TYPE
    exporting
      !EV_OBJTYPE type SWO_OBJTYP
      !EV_CARDNUM type /DFS/CARDNUM
    exceptions
      OBJTYPE_NOT_DEFINED .
  class-methods GET_CONNECTION
    exporting
      !ES_CONNECT type ZEDI_CONNECT
    exceptions
      DATA_NOT_FOUND .
  class-methods SIGN_FILE
    importing
      !IV_FILE_PATH type STRING
      !IV_JAVA_PARAMETER type STRING optional
      !IV_SYNCHRONOUS type STRING
    exceptions
      CNTL_ERROR
      ERROR_NO_GUI
      BAD_PARAMETER
      FILE_NOT_FOUND
      PATH_NOT_FOUND
      FILE_EXTENSION_UNKNOWN
      ERROR_EXECUTE_FAILED
      SYNCHRONOUS_FAILED
      NOT_SUPPORTED_BY_GUI
      CONNECTION_PARAMS_NOT_FOUND .
  class-methods CHANGE_XML_VALUE
    importing
      !IV_TAG_NAME type STRING
      !IV_TAG_VALUE type ANY
    changing
      !CV_XML_DATA type XSTRING .
protected section.
*"* protected components of class ZEDI_CL_MESSAGES
*"* do not include other source files here!!!
private section.
*"* private components of class ZEDI_CL_MESSAGES
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZEDI_CL_MESSAGES IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZEDI_CL_MESSAGES=>CER_IMPORT
* +-------------------------------------------------------------------------------------------------+
* | [EXC!] SETTINGS_NOT_FOUND
* | [EXC!] CERTIFICATE_IMPORT_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD cer_import.

  DATA: ls_cert_set    TYPE zedi_cert_set,
        lv_application TYPE string.

* Проверим существуют ли настройки для конкретного пользователя
  SELECT SINGLE * FROM zedi_cert_set INTO ls_cert_set
    WHERE uname = sy-uname.

* Если не нашли, то используем общие настройки
  IF ls_cert_set IS INITIAL.
    SELECT SINGLE * FROM zedi_cert_set INTO ls_cert_set
      WHERE uname = ''.

    IF sy-subrc NE 0.
      RAISE settings_not_found.
    ENDIF.
  ENDIF.

  IF ls_cert_set-cer_path_import IS NOT INITIAL.
    lv_application = ls_cert_set-cer_path_import.
    CONCATENATE '"' lv_application '"' INTO lv_application.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        application            = lv_application
        synchronous            = 'X'
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

    IF sy-subrc NE 0.
      RAISE certificate_import_error.
    ENDIF.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZEDI_CL_MESSAGES=>CHANGE_XML_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TAG_NAME                    TYPE        STRING
* | [--->] IV_TAG_VALUE                   TYPE        ANY
* | [<-->] CV_XML_DATA                    TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD change_xml_value.

    DATA: lv_xml_data TYPE xstring.

    DATA: lv_str_beg TYPE string,
          lv_str_end TYPE string,
          lv_str_dat TYPE string.

    DATA: lv_xstr_beg TYPE xstring,
          lv_xstr_end TYPE xstring,
          lv_xstr_dat TYPE xstring.

    DATA: lv_numl   TYPE int4,
          lv_numr   TYPE int4,
          ls_result TYPE match_result.

    IF cv_xml_data IS INITIAL.
      RETURN.
    ENDIF.

    lv_xml_data = cv_xml_data.

    CONCATENATE '<'  iv_tag_name '>' INTO lv_str_beg.
    CONCATENATE '</' iv_tag_name '>' INTO lv_str_end.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_str_beg
      IMPORTING
        buffer = lv_xstr_beg
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_str_end
      IMPORTING
        buffer = lv_xstr_end
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    lv_str_dat = iv_tag_value.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_str_dat
      IMPORTING
        buffer = lv_xstr_dat
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    FIND FIRST OCCURRENCE OF lv_xstr_beg IN lv_xml_data IN BYTE MODE RESULTS ls_result.
    IF sy-subrc = 0.
      lv_numl = ls_result-offset + ls_result-length.
    ENDIF.

    FIND FIRST OCCURRENCE OF lv_xstr_end IN lv_xml_data IN BYTE MODE RESULTS ls_result.
    IF sy-subrc = 0.
      lv_numr = ls_result-offset.
    ENDIF.

    IF lv_numl IS NOT INITIAL AND lv_numr IS NOT INITIAL.
      CONCATENATE lv_xml_data(lv_numl) lv_xstr_dat lv_xml_data+lv_numr INTO lv_xml_data IN BYTE MODE.
    ENDIF.

*   Удалим информацию о пространстве имен из тегов
    IF lv_xml_data IS NOT INITIAL.
      CALL TRANSFORMATION zedi_remove_namespaces_xslt
        SOURCE XML lv_xml_data
        RESULT XML lv_xml_data.
    ENDIF.

    cv_xml_data = lv_xml_data.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZEDI_CL_MESSAGES->CHECK_CERTIFICATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE_PATH                   TYPE        STRING
* | [EXC!] INVALID_FILE_PATH
* | [EXC!] UPLOAD_ERROR
* | [EXC!] DATA_NOT_BOUND
* | [EXC!] INVALID_CERTIFICATE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_certificate.

    CONSTANTS:
      lc_security_party_id1(512) TYPE c VALUE 'Унитарное предприятие',                "'Унитарное предприятие по оказанию услуг "А1"'
      lc_security_party_id2(512) TYPE c VALUE 'ГосСУОК',                              "'Республиканский удостоверяющий центр ГосСУОК'
      lc_security_party_id3(512) TYPE c VALUE 'Национальный центр электронных услуг'. "'Республиканское унитарное предприятие "Национальный центр электронных услуг"'

    DATA: lv_xml_data  TYPE xstring,
          ls_msg_data  TYPE REF TO data,
          lv_file_path TYPE string.

    FIELD-SYMBOLS: <ft_security> TYPE table,
                   <fs_security> TYPE any,
                   <ls_msg_type> TYPE any,
                   <ls_msg_data> TYPE any,
                   <ls_cmp>      TYPE any.

    DEFINE macros_check_security_party_id.
      IF &1 IS NOT INITIAL AND &1 NS lc_security_party_id1
                           AND &1 NS lc_security_party_id2
                           AND &1 NS lc_security_party_id3.
        " Неверный сертификат!
        RAISE invalid_certificate.
      ENDIF.
    END-OF-DEFINITION.

*   Создание данных нужного типа
    lv_file_path = iv_file_path.
    CREATE DATA ls_msg_data TYPE (me->proxy_data_type).
    IF ls_msg_data IS BOUND.
      ASSIGN ls_msg_data->* TO <ls_msg_data>.
    ELSE.
      RAISE data_not_bound.
    ENDIF.

*   Загрузим данные файла с подписью
    CALL METHOD me->upload_msg_from_file
      EXPORTING
        iv_upload_type            = zedi_if_file_load_type=>frontend
        iv_file_path              = lv_file_path
      IMPORTING
        ev_file_data              = lv_xml_data
      EXCEPTIONS
        invalid_file_path         = 1
        upload_error              = 2
        upload_type_not_supported = 3
        OTHERS                    = 4.
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1.
          RAISE invalid_file_path.
        WHEN OTHERS.
          RAISE upload_error.
      ENDCASE.
    ENDIF.

*   Проверим сертификат подписи
    IF lv_xml_data IS NOT INITIAL.
      CALL METHOD me->get_input_message
        EXPORTING
          iv_xml_data = lv_xml_data
        IMPORTING
          es_msg_data = <ls_msg_data>.

      ASSIGN COMPONENT 'MESSAGE_HEADER-MESSAGE_TYPE' OF STRUCTURE <ls_msg_data> TO <ls_msg_type>.
      CHECK sy-subrc = 0.

      CASE <ls_msg_type>.

        WHEN 'BLRWBL' OR 'BLRDLN'.
*         Подпись отправителя
          ASSIGN COMPONENT 'SECURITY_SHIPPER' OF STRUCTURE <ls_msg_data> TO <ft_security>.
          IF <ft_security> IS ASSIGNED.
            LOOP AT <ft_security> ASSIGNING <fs_security>.
              IF <fs_security> IS ASSIGNED.
                ASSIGN COMPONENT 'SECURITY_PARTY_ID' OF STRUCTURE <fs_security> TO <ls_cmp>.
                IF <ls_cmp> IS ASSIGNED.
                  macros_check_security_party_id <ls_cmp>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

        WHEN 'BLRWBR' OR 'BLRDLR'.
*         Подпись получателя
          ASSIGN COMPONENT 'SECURITY_RECEIVER-SECURITY_PARTY_ID' OF STRUCTURE <ls_msg_data> TO <ls_cmp>.
          IF <ls_cmp> IS ASSIGNED.
            macros_check_security_party_id <ls_cmp>.
          ENDIF.

        WHEN 'BLRINV'.
*         Подпись получателя
          ASSIGN COMPONENT 'SECURITY_BUYER' OF STRUCTURE <ls_msg_data> TO <ft_security>.
          IF <ft_security> IS ASSIGNED.
            LOOP AT <ft_security> ASSIGNING <fs_security>.
              IF <fs_security> IS ASSIGNED.
                ASSIGN COMPONENT 'SECURITY_PARTY_ID' OF STRUCTURE <fs_security> TO <ls_cmp>.
                IF <ls_cmp> IS ASSIGNED.
                  macros_check_security_party_id <ls_cmp>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

        WHEN 'BLRAPN'.
*         Подпись отправителя
          ASSIGN COMPONENT 'SECURITY_SHIPPER-SECURITY_PARTY_ID' OF STRUCTURE <ls_msg_data> TO <ls_cmp>.
          IF <ls_cmp> IS ASSIGNED.
            macros_check_security_party_id <ls_cmp>.
          ENDIF.

          UNASSIGN: <ls_cmp>.

*         Подпись получателя
          ASSIGN COMPONENT 'SECURITY_RECEIVER-SECURITY_PARTY_ID' OF STRUCTURE <ls_msg_data> TO <ls_cmp>.
          IF <ls_cmp> IS ASSIGNED.
            macros_check_security_party_id <ls_cmp>.
          ENDIF.

      ENDCASE.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZEDI_CL_MESSAGES->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MSG_TYPE                    TYPE        ZEDI_MSG_TYPE(optional)
* | [EXC!] INVALID_MESSAGE_TYPE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD constructor.

  me->message_type = iv_msg_type.

  CASE iv_msg_type.
    WHEN zedi_if_message_type=>blrwbl. me->proxy_data_type = 'ZEDI_WBL'.
    WHEN zedi_if_message_type=>blrwbr. me->proxy_data_type = 'ZEDI_WBR'.
    WHEN zedi_if_message_type=>blrdln. me->proxy_data_type = 'ZEDI_DLN'.
    WHEN zedi_if_message_type=>blrdnr. me->proxy_data_type = 'ZEDI_DNR'.
    WHEN zedi_if_message_type=>blrapn. me->proxy_data_type = 'ZEDI_APN'.
    WHEN zedi_if_message_type=>blrinv. me->proxy_data_type = 'ZEDI_INV'.
    WHEN zedi_if_message_type=>blrdoc. me->proxy_data_type = 'ZEDI_DOC'.
    WHEN OTHERS.
      me->proxy_data_type = iv_msg_type.
  ENDCASE.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZEDI_CL_MESSAGES=>CRL_IMPORT
* +-------------------------------------------------------------------------------------------------+
* | [EXC!] SETTINGS_NOT_FOUND
* | [EXC!] CERTIFICATE_IMPORT_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD crl_import.

  DATA: ls_cert_set    TYPE zedi_cert_set,
        lv_application TYPE string.

* Проверим существуют ли настройки для конкретного пользователя
  SELECT SINGLE * FROM zedi_cert_set INTO ls_cert_set
    WHERE uname = sy-uname.

* Если не нашли, то используем общие настройки
  IF ls_cert_set IS INITIAL.
    SELECT SINGLE * FROM zedi_cert_set INTO ls_cert_set
      WHERE uname = ''.

    IF sy-subrc NE 0.
      RAISE settings_not_found.
    ENDIF.
  ENDIF.

  IF ls_cert_set-crl_path_import IS NOT INITIAL.
    lv_application = ls_cert_set-crl_path_import.
    CONCATENATE '"' lv_application '"' INTO lv_application.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        application            = lv_application
        synchronous            = 'X'
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

    IF sy-subrc NE 0.
      RAISE certificate_import_error.
    ENDIF.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZEDI_CL_MESSAGES=>DOWNLOAD_MSG_TO_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DOWNLOAD_TYPE               TYPE        STRING (default =ZEDI_IF_FILE_LOAD_TYPE=>SCREEN)
* | [--->] IV_FILE_PATH                   TYPE        STRING(optional)
* | [--->] IV_FILE_DATA                   TYPE        XSTRING
* | [EXC!] INVALID_FILE_PATH
* | [EXC!] DOWNLOAD_ERROR
* | [EXC!] DOWNLOAD_TYPE_NOT_SUPPORTED
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD download_msg_to_file.

  CASE iv_download_type.
    WHEN zedi_if_file_load_type=>server.                        " Выгрузка на сервер

      IF iv_file_path IS NOT INITIAL.
        OPEN DATASET iv_file_path FOR OUTPUT IN BINARY MODE.
        IF sy-subrc = 0.
          TRANSFER iv_file_data TO iv_file_path.
          CLOSE DATASET iv_file_path.
        ELSE.
          RAISE download_error.
        ENDIF.
      ELSE.
        RAISE invalid_file_path.
      ENDIF.

    WHEN zedi_if_file_load_type=>frontend.                      " Выгрузка на фронтэнд

      IF iv_file_path IS NOT INITIAL.
        CALL METHOD cl_proxy_service=>download_payload
          EXPORTING
            payload = iv_file_data
            file_x  = iv_file_path.
        IF sy-subrc NE 0.
          RAISE download_error.
        ENDIF.
      ELSE.
        RAISE invalid_file_path.
      ENDIF.

    WHEN zedi_if_file_load_type=>screen.                        " Вывод на экран

      CALL METHOD cl_srtg_helper=>write_utf8_xmldoc
        EXPORTING
          doc              = iv_file_data
          use_html_control = tsoap_true.
      IF sy-subrc NE 0.
        RAISE download_error.
      ENDIF.

    WHEN OTHERS.
      RAISE download_type_not_supported.
  ENDCASE.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZEDI_CL_MESSAGES=>GET_CARD_OBJTYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MSG_TYPE                    TYPE        ZEDI_MSG_TYPE
* | [<---] EV_OBJTYPE                     TYPE        SWO_OBJTYP
* | [<---] EV_CARDNUM                     TYPE        /DFS/CARDNUM
* | [EXC!] OBJTYPE_NOT_DEFINED
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_card_objtype.

    CASE iv_msg_type.
      WHEN 'BLRWBL' OR 'BLRWBR' OR 'BLRDLN' OR 'BLRDNR'.
        ev_objtype = z030_if_030003_constants=>main_bo_name.
        ev_cardnum = z030_if_030003_constants=>card_number.
      WHEN 'BLRINV'.
        ev_objtype = z030_if_030004_constants=>main_bo_name.
        ev_cardnum = z030_if_030004_constants=>card_number.
      WHEN 'BLRDOC'.
        ev_objtype = z030_if_030005_constants=>main_bo_name.
        ev_cardnum = z030_if_030005_constants=>card_number.
      WHEN OTHERS.
        RAISE objtype_not_defined.
    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZEDI_CL_MESSAGES=>GET_CONNECTION
* +-------------------------------------------------------------------------------------------------+
* | [<---] ES_CONNECT                     TYPE        ZEDI_CONNECT
* | [EXC!] DATA_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_connection.

    FIELD-SYMBOLS: <fs_field> TYPE any.

*   Проверим существуют ли настройки для конкретного пользователя
    SELECT SINGLE * FROM zedi_connect INTO es_connect
      WHERE uname = sy-uname.

*   Если не нашли, то используем общие настройки
    IF es_connect IS INITIAL.
      SELECT SINGLE * FROM zedi_connect INTO es_connect
        WHERE uname = ''.
    ENDIF.

    IF es_connect IS INITIAL.
      RAISE data_not_found.
    ELSE.
      WHILE sy-subrc = 0.
        IF <fs_field> IS ASSIGNED.
          REPLACE ALL OCCURRENCES OF '<sy-sysid>' IN <fs_field> WITH sy-sysid.
        ENDIF.
        ASSIGN COMPONENT sy-index OF STRUCTURE es_connect TO <fs_field>.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZEDI_CL_MESSAGES->GET_INPUT_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_XML_DATA                    TYPE        XSTRING
* | [<---] ES_MSG_DATA                    TYPE        ANY
* | [EXC!] PROXY_FAULT
* | [EXC!] TRANSFORMATION_ERROR
* | [EXC!] CRITICAL_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_input_message.

  TRY.

*     Преобразуем XML в ABAP-структуру
      CALL METHOD cl_proxy_xml_transform=>xml_xstring_to_abap
        EXPORTING
          ddic_type = proxy_data_type
          xml       = iv_xml_data
        IMPORTING
          abap_data = es_msg_data.

    CATCH cx_proxy_fault.
      RAISE proxy_fault.

    CATCH cx_transformation_error.
      RAISE transformation_error.

    CATCH cx_root.
      RAISE critical_error.

  ENDTRY.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZEDI_CL_MESSAGES=>GET_MSG_ID_PARAMETERS
* +-------------------------------------------------------------------------------------------------+
* | [<---] EV_MSG_DATE_TIME               TYPE        C
* | [<---] EV_MSG_ID                      TYPE        C
* | [<---] EV_DOC_ID                      TYPE        C
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_msg_id_parameters.

    DATA: lv_time_stamp       TYPE timestampl,
          lv_time_stamp_c(29) TYPE c.

*   В качестве ID исползуем штамп даты до миллисекунд (например, 31.12.2015 23:59:59:01 = 15123123595901)
    GET TIME STAMP FIELD lv_time_stamp.
    lv_time_stamp_c = lv_time_stamp.
    REPLACE ALL OCCURRENCES OF '.' IN lv_time_stamp_c WITH ''.
    CONDENSE lv_time_stamp_c.

    ev_msg_date_time = lv_time_stamp_c(14).
    ev_msg_id = lv_time_stamp_c+2(14).
    ev_doc_id = lv_time_stamp_c+2(14).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZEDI_CL_MESSAGES->GET_OUTPUT_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MSG_DATA                    TYPE        ANY
* | [<---] EV_XML_DATA                    TYPE        XSTRING
* | [EXC!] PROXY_FAULT
* | [EXC!] TRANSFORMATION_ERROR
* | [EXC!] CRITICAL_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_output_message.

  DATA: lv_xml_data TYPE xstring.

  TRY.

*     Преобразуем ABAP-структуру в XML
      CALL METHOD cl_proxy_xml_transform=>abap_to_xml_xstring
        EXPORTING
          abap_data  = is_msg_data
          ddic_type  = proxy_data_type
          xml_header = 'full'
        RECEIVING
          xml        = lv_xml_data.

*     Удалим информацию о пространстве имен из тегов
      IF lv_xml_data IS NOT INITIAL.
        CALL TRANSFORMATION zedi_remove_namespaces_xslt
          SOURCE XML lv_xml_data
          RESULT XML lv_xml_data.
      ENDIF.

      ev_xml_data = lv_xml_data.

    CATCH cx_proxy_fault.
      RAISE proxy_fault.

    CATCH cx_transformation_error.
      RAISE transformation_error.

    CATCH cx_root.
      RAISE critical_error.

  ENDTRY.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZEDI_CL_MESSAGES->SEND_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE_NAME                   TYPE        STRING
* | [--->] IV_FILE_PATH                   TYPE        STRING
* | [EXC!] CONNECTION_PARAMETERS_ERROR
* | [EXC!] SEND_MESSAGE_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_message.

    DATA: ls_connect TYPE zedi_connect.

    DATA: lv_file_path     TYPE string,
          lv_file_path_out TYPE string.

*   Получим параметры подключения
    CALL METHOD me->get_connection
      IMPORTING
        es_connect     = ls_connect
      EXCEPTIONS
        data_not_found = 1
        OTHERS         = 2.

    IF sy-subrc NE 0 OR ls_connect IS INITIAL.
      " Ошибка получения параметров подключения!
      RAISE connection_parameters_error.
    ENDIF.

    lv_file_path = iv_file_path.

    CONCATENATE ls_connect-connector_path
                ls_connect-connector_dir_out
                iv_file_name INTO lv_file_path_out.

*   Скопируем файл в папку отправки
    CALL METHOD cl_gui_frontend_services=>file_copy
      EXPORTING
        source               = lv_file_path
        destination          = lv_file_path_out
        overwrite            = space
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        disk_full            = 4
        access_denied        = 5
        file_not_found       = 6
        destination_exists   = 7
        unknown_error        = 8
        path_not_found       = 9
        disk_write_protect   = 10
        drive_not_ready      = 11
        not_supported_by_gui = 12
        OTHERS               = 13.
    IF sy-subrc <> 0.
      " Ошибка отправки сообщения!
      RAISE send_message_error.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZEDI_CL_MESSAGES=>SIGN_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE_PATH                   TYPE        STRING
* | [--->] IV_JAVA_PARAMETER              TYPE        STRING(optional)
* | [--->] IV_SYNCHRONOUS                 TYPE        STRING
* | [EXC!] CNTL_ERROR
* | [EXC!] ERROR_NO_GUI
* | [EXC!] BAD_PARAMETER
* | [EXC!] FILE_NOT_FOUND
* | [EXC!] PATH_NOT_FOUND
* | [EXC!] FILE_EXTENSION_UNKNOWN
* | [EXC!] ERROR_EXECUTE_FAILED
* | [EXC!] SYNCHRONOUS_FAILED
* | [EXC!] NOT_SUPPORTED_BY_GUI
* | [EXC!] CONNECTION_PARAMS_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD sign_file.

    DATA: lv_java_path_local TYPE string,
          lv_sign_path_local TYPE string.

    DATA: ls_connect TYPE zedi_connect.

    DATA: lv_parameter TYPE string,
          lv_java_path TYPE string,
          lv_sign_path TYPE string,
          lv_file_path TYPE string.

    DATA: lv_default_directory TYPE string,
          lv_result.

*   Получим параметры подключения
    CALL METHOD zedi_cl_messages=>get_connection
      IMPORTING
        es_connect     = ls_connect
      EXCEPTIONS
        data_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      RAISE connection_params_not_found.
    ENDIF.

*   Импортируем СОС в личное хранилище
    CALL METHOD zedi_cl_messages=>crl_import
      EXCEPTIONS
        settings_not_found       = 1
        certificate_import_error = 2
        OTHERS                   = 3.

*   Параметры сетевого запуска EDSSigner
    lv_java_path = ls_connect-java_path.
    lv_sign_path = ls_connect-signer_path.
    lv_default_directory = lv_sign_path.

*   Проверим локальную установку EDSSigner
    IF ls_connect-uname IS INITIAL.
      lv_java_path_local = ls_connect-java_path_local.
      lv_sign_path_local = ls_connect-signer_path_local.

      CALL METHOD cl_gui_frontend_services=>directory_exist
        EXPORTING
          directory            = lv_sign_path_local
        RECEIVING
          result               = lv_result
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5.

      IF lv_result IS NOT INITIAL.
        lv_java_path = lv_java_path_local.
        lv_sign_path = lv_sign_path_local.

*       Если запуск EDSSigner локальный (для VDI), то импортируем сертификат в личное хранилище
        CALL METHOD zedi_cl_messages=>cer_import
          EXCEPTIONS
            settings_not_found       = 1
            certificate_import_error = 2
            OTHERS                   = 3.

      ENDIF.
    ENDIF.

    CONCATENATE lv_sign_path '\EDSSigner.jar' INTO lv_sign_path.

    CONCATENATE '"' lv_java_path '"' INTO lv_java_path.
    CONCATENATE '"' lv_sign_path '"' INTO lv_sign_path.

    lv_file_path = iv_file_path.
    IF lv_file_path(1) NE '"'.
      CONCATENATE '"' lv_file_path '"' INTO lv_file_path.
    ENDIF.

    CONCATENATE iv_java_parameter '-jar' lv_sign_path '-sign' lv_file_path INTO lv_parameter SEPARATED BY space.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        application            = lv_java_path
        parameter              = lv_parameter
        default_directory      = lv_default_directory
        synchronous            = iv_synchronous
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

    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1.
          RAISE cntl_error.
        WHEN 2.
          RAISE error_no_gui.
        WHEN 3.
          RAISE bad_parameter.
        WHEN 4.
          RAISE file_not_found.
        WHEN 5.
          RAISE path_not_found.
        WHEN 6.
          RAISE file_extension_unknown.
        WHEN 7.
          RAISE error_execute_failed.
        WHEN 8.
          RAISE synchronous_failed.
        WHEN 9.
          RAISE not_supported_by_gui.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZEDI_CL_MESSAGES->SIGN_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MSG_DATA                    TYPE        ANY
* | [--->] IV_XML_DATA                    TYPE        XSTRING(optional)
* | [<-->] CV_FILE_NAME                   TYPE        STRING(optional)
* | [<-->] CV_FILE_PATH                   TYPE        STRING(optional)
* | [EXC!] CONNECTION_PARAMETERS_ERROR
* | [EXC!] DOWNLOAD_FILE_ERROR
* | [EXC!] SIGN_FILE_ERROR
* | [EXC!] CREATE_MESSAGE_ERROR
* | [EXC!] CHECK_CERTIFICATE_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD sign_message.

    DATA: ls_connect        TYPE zedi_connect,
          lv_java_parameter TYPE string.

    DATA: lv_file_path TYPE string,
          lv_file_name TYPE string,
          lv_xml_data  TYPE xstring,
          lv_result.

    FIELD-SYMBOLS: <ls_msg> TYPE any,
                   <ls_cmp> TYPE any.

*   Имя файла
    ASSIGN is_msg_data TO <ls_msg>.
    IF <ls_msg> IS ASSIGNED.
      ASSIGN COMPONENT 'MESSAGE_HEADER-MESSAGE_ID' OF STRUCTURE <ls_msg> TO <ls_cmp>.
      IF <ls_cmp> IS ASSIGNED.
        CONCATENATE lv_file_name <ls_cmp> '_' INTO lv_file_name.
      ENDIF.
      ASSIGN COMPONENT 'MESSAGE_HEADER-MSG_RECEIVER_ID' OF STRUCTURE <ls_msg> TO <ls_cmp>.
      IF <ls_cmp> IS ASSIGNED.
        CONCATENATE lv_file_name <ls_cmp> '_' INTO lv_file_name.
      ENDIF.
      ASSIGN COMPONENT 'MESSAGE_HEADER-MSG_SENDER_ID' OF STRUCTURE <ls_msg> TO <ls_cmp>.
      IF <ls_cmp> IS ASSIGNED.
        CONCATENATE lv_file_name <ls_cmp> '.xml' INTO lv_file_name.
      ENDIF.
    ENDIF.

*   Получим параметры подключения
    CALL METHOD me->get_connection
      IMPORTING
        es_connect     = ls_connect
      EXCEPTIONS
        data_not_found = 1
        OTHERS         = 2.
    IF sy-subrc NE 0 OR ls_connect IS INITIAL.
      RAISE connection_parameters_error.
    ENDIF.

    CONCATENATE ls_connect-signer_path
                ls_connect-signer_dir_in
                lv_file_name INTO lv_file_path.

*   Выгрузим и подпишем исходящий файл
    IF iv_xml_data IS NOT INITIAL.
      lv_xml_data = iv_xml_data.
    ELSE.
      CALL METHOD me->get_output_message
        EXPORTING
          is_msg_data = is_msg_data
        IMPORTING
          ev_xml_data = lv_xml_data.
    ENDIF.

    IF lv_xml_data IS NOT INITIAL.
      CALL METHOD me->download_msg_to_file
        EXPORTING
          iv_download_type            = zedi_if_file_load_type=>frontend
          iv_file_path                = lv_file_path
          iv_file_data                = lv_xml_data
        EXCEPTIONS
          invalid_file_path           = 1
          download_error              = 2
          download_type_not_supported = 3
          OTHERS                      = 4.
      IF sy-subrc NE 0.
        RAISE download_file_error.
      ENDIF.

      IF me->message_type = zedi_if_message_type=>blrdoc.
        lv_java_parameter = '-Xmx512m'.
      ENDIF.

      CALL METHOD me->sign_file
        EXPORTING
          iv_file_path           = lv_file_path
          iv_java_parameter      = lv_java_parameter
          iv_synchronous         = 'X'
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
      IF sy-subrc NE 0.
        RAISE sign_file_error.
      ENDIF.
    ENDIF.

*   Проверим создание подписанного файла
    REPLACE ls_connect-signer_dir_in IN lv_file_path WITH ls_connect-signer_dir_out.
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = lv_file_path
      RECEIVING
        result               = lv_result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.

    IF sy-subrc NE 0 OR lv_result IS INITIAL.
      RAISE create_message_error.
    ELSE.
*     Проверим корректность сертификата для подписи
      CALL METHOD me->check_certificate
        EXPORTING
          iv_file_path        = lv_file_path
        EXCEPTIONS
          invalid_file_path   = 1
          upload_error        = 2
          invalid_certificate = 3
          OTHERS              = 4.
      IF sy-subrc <> 0.
        RAISE check_certificate_error.
      ENDIF.
    ENDIF.

*   Обновим возвращаемые параметры
    cv_file_name = lv_file_name.
    cv_file_path = lv_file_path.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZEDI_CL_MESSAGES=>UPLOAD_MSG_FROM_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UPLOAD_TYPE                 TYPE        STRING (default =ZEDI_IF_FILE_LOAD_TYPE=>FRONTEND)
* | [--->] IV_FILE_PATH                   TYPE        STRING
* | [<---] EV_FILE_DATA                   TYPE        XSTRING
* | [EXC!] INVALID_FILE_PATH
* | [EXC!] UPLOAD_ERROR
* | [EXC!] UPLOAD_TYPE_NOT_SUPPORTED
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD upload_msg_from_file.

  CASE iv_upload_type.
    WHEN zedi_if_file_load_type=>server.                        " Выгрузка на сервер

      IF iv_file_path IS NOT INITIAL.
        OPEN DATASET iv_file_path FOR INPUT IN BINARY MODE.
        IF sy-subrc = 0.
          READ DATASET iv_file_path INTO ev_file_data.
          CLOSE DATASET iv_file_path.
        ELSE.
          RAISE upload_error.
        ENDIF.
      ELSE.
        RAISE invalid_file_path.
      ENDIF.

    WHEN zedi_if_file_load_type=>frontend.                      " Выгрузка на фронтэнд

      IF iv_file_path IS NOT INITIAL.
        CALL METHOD cl_proxy_service=>gui_upload
          EXPORTING
            filename = iv_file_path
          RECEIVING
            result   = ev_file_data.
        IF sy-subrc NE 0.
          RAISE upload_error.
        ENDIF.
      ELSE.
        RAISE invalid_file_path.
      ENDIF.

    WHEN OTHERS.
      RAISE upload_type_not_supported.
  ENDCASE.

ENDMETHOD.
ENDCLASS.
