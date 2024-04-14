*&---------------------------------------------------------------------*
*&  Include           ZEDI_DFS_MANAGER_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  get_connection
*&---------------------------------------------------------------------*
FORM get_connection.

  CALL METHOD zedi_cl_messages=>get_connection
    IMPORTING
      es_connect     = gs_connect
    EXCEPTIONS
      data_not_found = 1
      OTHERS         = 2.

  IF sy-subrc = 0.
    CONCATENATE gs_connect-repository_path
                gs_connect-repository_dir_in INTO p_dname.
  ENDIF.

ENDFORM.                    "get_connection

*&---------------------------------------------------------------------*
*&      Form  check_running_jobs
*&---------------------------------------------------------------------*
FORM check_running_jobs.

  DATA: lt_joblist   TYPE TABLE OF tbtcjob_bk,
        ls_btcselect TYPE btcselect.

  ls_btcselect-running  = 'X'.
  ls_btcselect-jobname  = '*'.
  ls_btcselect-username = '*'.
  ls_btcselect-abapname = sy-cprog.

  CALL FUNCTION 'BP_JOB_SELECT_SM37B'
    EXPORTING
      jobselect_dialog    = 'N'
      jobsel_param_in     = ls_btcselect
    TABLES
      jobselect_joblist_b = lt_joblist
    EXCEPTIONS
      invalid_dialog_type = 1
      jobname_missing     = 2
      no_jobs_found       = 3
      selection_canceled  = 4
      username_missing    = 5
      OTHERS              = 6.

  IF sy-subrc = 0.
    IF sy-batch IS INITIAL.
      IF lines( lt_joblist ) > 0.
        MESSAGE e000(z030_edi) WITH 'Существуют активные фоновые' 'задания для программы' sy-cprog '!'.
      ENDIF.
    ELSE.
      IF lines( lt_joblist ) > 1. " Может быть только одно фоновое задание - текущее
        LEAVE PROGRAM.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "check_running_jobs

*&---------------------------------------------------------------------*
*&      Form  get_file_list
*&---------------------------------------------------------------------*
FORM get_file_list TABLES ct_files     TYPE tt_file_list
                    USING iv_dir_name  TYPE string
                          iv_file_name TYPE string.

  DATA: lv_count      TYPE i,
        lt_file_table TYPE file_info OCCURS 0  WITH HEADER LINE,

        lv_dir_name   TYPE eps2filnam,
        lt_dir_list   TYPE eps2fili OCCURS 0 WITH HEADER LINE.

  REFRESH ct_files[].

  IF iv_file_name IS INITIAL.
    CASE abap_on.
      WHEN p_work.
        CALL METHOD cl_gui_frontend_services=>directory_list_files
          EXPORTING
            directory                   = iv_dir_name
            filter                      = '*.xml'
            files_only                  = 'X'
          CHANGING
            file_table                  = lt_file_table[]
            count                       = lv_count
          EXCEPTIONS
            cntl_error                  = 1
            directory_list_files_failed = 2
            wrong_parameter             = 3
            error_no_gui                = 4
            not_supported_by_gui        = 5
            OTHERS                      = 6.

        IF sy-subrc = 0.
          LOOP AT lt_file_table.
            ct_files-filename = lt_file_table-filename.
            APPEND ct_files.
          ENDLOOP.
        ENDIF.
      WHEN p_serv.
        lv_dir_name = iv_dir_name.

        CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
          EXPORTING
            iv_dir_name            = lv_dir_name
          TABLES
            dir_list               = lt_dir_list
          EXCEPTIONS
            invalid_eps_subdir     = 1
            sapgparam_failed       = 2
            build_directory_failed = 3
            no_authorization       = 4
            read_directory_failed  = 5
            too_many_read_errors   = 6
            empty_directory_list   = 7
            OTHERS                 = 8.

        IF sy-subrc = 0.
          LOOP AT lt_dir_list.
            ct_files-filename = lt_dir_list-name.
            APPEND ct_files.
          ENDLOOP.
        ENDIF.
    ENDCASE.
  ELSE.
    ct_files-filename = iv_file_name.
    APPEND ct_files.
  ENDIF.

ENDFORM.                    "get_file_list

*&---------------------------------------------------------------------*
*&      Form  move_file_to_archive
*&---------------------------------------------------------------------*
FORM move_file_to_archive USING iv_file_path TYPE string
                                iv_file_path_archive TYPE string.

  DATA: lv_rc            TYPE i,
        lv_xline         TYPE xstring,
        lv_command(1000) TYPE c,
        lv_error         TYPE string.

  CASE abap_on.
    WHEN p_work.
      CALL METHOD cl_gui_frontend_services=>file_copy
        EXPORTING
          source               = iv_file_path
          destination          = iv_file_path_archive
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

      IF sy-subrc = 0.
        CALL METHOD cl_gui_frontend_services=>file_delete
          EXPORTING
            filename             = iv_file_path
          CHANGING
            rc                   = lv_rc
          EXCEPTIONS
            file_delete_failed   = 1
            cntl_error           = 2
            error_no_gui         = 3
            file_not_found       = 4
            access_denied        = 5
            unknown_error        = 6
            not_supported_by_gui = 7
            wrong_parameter      = 8
            OTHERS               = 9.
      ENDIF.

      IF sy-subrc NE 0.
        CONCATENATE 'Ошибка перемещения файла:' iv_file_path '!' INTO gs_errors-line SEPARATED BY space.
        APPEND gs_errors TO gt_errors.
      ENDIF.
    WHEN p_serv.

***      CONCATENATE 'mv' iv_file_path iv_file_path_archive INTO lv_command SEPARATED BY space.
***      CONDENSE lv_command.
***
***      CALL 'SYSTEM' ID 'COMMAND' FIELD lv_command.
***      WAIT UP TO 1 SECONDS.

      OPEN DATASET iv_file_path FOR INPUT IN BINARY MODE.
      IF sy-subrc = 0.
        OPEN DATASET iv_file_path_archive FOR OUTPUT IN BINARY MODE.
        IF sy-subrc = 0.

          DO.
            READ DATASET iv_file_path INTO lv_xline.
            IF sy-subrc = 0.
              TRANSFER lv_xline TO iv_file_path_archive.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.

          CLOSE DATASET iv_file_path.
          CLOSE DATASET iv_file_path_archive.

          DELETE DATASET iv_file_path.
        ELSE.
          CLOSE DATASET iv_file_path.
        ENDIF.
      ENDIF.

*     Попробуем открыть конечный файл
      OPEN DATASET iv_file_path_archive FOR INPUT IN BINARY MODE.
      IF sy-subrc = 0.
        CLOSE DATASET iv_file_path_archive.
      ELSE.
*       Если не смогли открыть файл, значит он не переместился, и это ошибка
        CONCATENATE 'Ошибка перемещения файла:' iv_file_path '!' INTO gs_errors-line SEPARATED BY space.
        APPEND gs_errors TO gt_errors.
        EXIT.
      ENDIF.

*     Попробуем открыть исходный файл
      OPEN DATASET iv_file_path FOR INPUT IN BINARY MODE.
      IF sy-subrc = 0.
        CLOSE DATASET iv_file_path.
*       Если смогли открыть файл, значит он не переместился, и это ошибка
        CONCATENATE 'Ошибка перемещения файла:' iv_file_path '!' INTO gs_errors-line SEPARATED BY space.
        APPEND gs_errors TO gt_errors.
        EXIT.
      ENDIF.
  ENDCASE.

ENDFORM.                    "move_file_to_archive

*&---------------------------------------------------------------------*
*&      Form  get_message_type
*&---------------------------------------------------------------------*
FORM get_message_type USING iv_file     TYPE string
                   CHANGING cv_msg_type TYPE zedi_msg_type.

  DATA: lv_xml_size   TYPE i,
        lv_xml_line   TYPE string,
        lt_xml_table  TYPE tt_xml_line,

        lref_parser   TYPE REF TO if_ixml_parser,
        lref_document TYPE REF TO if_ixml_document.

* Прочитаем файл
  PERFORM read_file USING iv_file
                 CHANGING lv_xml_line lv_xml_size.

* Получим xml-данные
  PERFORM read_xml USING lt_xml_table lv_xml_line lv_xml_size
                CHANGING lref_parser lref_document.

* Получим данные для обновления
  IF lref_parser IS NOT INITIAL AND lref_document IS NOT INITIAL.
    PERFORM get_xml_data USING lref_parser lref_document
                      CHANGING cv_msg_type.
  ENDIF.

ENDFORM.                    "get_message_type

*&---------------------------------------------------------------------*
*&      Form  read_file
*&---------------------------------------------------------------------*
*       Read files data (server)
*----------------------------------------------------------------------*
*      -->IV_FILENAME  File name
*      <--CV_XML_LINE  File data
*      <--CV_XML_SIZE  File size
*----------------------------------------------------------------------*
FORM read_file USING iv_filename TYPE string
            CHANGING cv_xml_line TYPE string
                     cv_xml_size TYPE i.

  DATA: lt_data TYPE string OCCURS 0 WITH HEADER LINE,
        lv_line TYPE string,
        lv_size TYPE i.

  CASE abap_on.
    WHEN p_work.

      CALL METHOD cl_gui_frontend_services=>gui_upload
        EXPORTING
          filename                = iv_filename
        IMPORTING
          filelength              = cv_xml_size
        CHANGING
          data_tab                = lt_data[]
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          not_supported_by_gui    = 17
          error_no_gui            = 18
          OTHERS                  = 19.

      IF sy-subrc = 0.
        LOOP AT lt_data.
          CONCATENATE cv_xml_line lt_data INTO cv_xml_line.
        ENDLOOP.
      ENDIF.

    WHEN p_serv.
      OPEN DATASET iv_filename FOR INPUT IN TEXT MODE ENCODING UTF-8.

      IF sy-subrc = 0.
        DO.
          READ DATASET iv_filename INTO lv_line LENGTH lv_size.
          IF sy-subrc = 0.
            CONCATENATE cv_xml_line lv_line INTO cv_xml_line.
            cv_xml_size = cv_xml_size + lv_size.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        CLOSE DATASET iv_filename.
      ENDIF.

  ENDCASE.

ENDFORM.                    " read_file

*&---------------------------------------------------------------------*
*&      Form  READ_XML
*&---------------------------------------------------------------------*
*       Create document and parser XML
*----------------------------------------------------------------------*
*      -->IT_XML_TABLE  XML data (table)
*      -->IV_XML_LINE   XML data (string line)
*      -->IV_XML_SIZE   Size of XML data
*      <--CREF_PARSER   Parser XML
*      <--CREF_DOCUMENT Document XML
*----------------------------------------------------------------------*
FORM read_xml USING it_xml_table  TYPE tt_xml_line
                    iv_xml_line   TYPE string
                    iv_xml_size   TYPE i
           CHANGING cref_parser   TYPE REF TO if_ixml_parser
                    cref_document TYPE REF TO if_ixml_document.

  DATA: lref_ixml          TYPE REF TO if_ixml,
        lref_streamfactory TYPE REF TO if_ixml_stream_factory,
        lref_parser        TYPE REF TO if_ixml_parser,
        lref_istream       TYPE REF TO if_ixml_istream,
        lref_document      TYPE REF TO if_ixml_document,
        lref_parse_error   TYPE REF TO if_ixml_parse_error,
        lv_error           TYPE c,
        lv_error_num       TYPE i,
        lv_index           TYPE i,
        lv_str             TYPE string.

* Creating the main iXML factory
  lref_ixml = cl_ixml=>create( ).

* Creating a stream factory
  lref_streamfactory = lref_ixml->create_stream_factory( ).

* Wrap the table containing the file into a stream
  IF NOT it_xml_table IS INITIAL AND NOT iv_xml_size IS INITIAL.
    lref_istream = lref_streamfactory->create_istream_itable( table = it_xml_table
                                                               size = iv_xml_size ).
  ELSEIF NOT iv_xml_line IS INITIAL.
    lref_istream = lref_streamfactory->create_istream_cstring( string = iv_xml_line ).
  ELSE.
    lv_error = 'X'.
  ENDIF.

  IF lv_error IS INITIAL.
*   creating a document
    lref_document = lref_ixml->create_document( ).

*   Create a Parser
    lref_parser = lref_ixml->create_parser( stream_factory = lref_streamfactory
                                                   istream = lref_istream
                                                  document = lref_document ).
*   Parse the stream
    lv_error_num = lref_parser->parse( ).
    IF lv_error_num <> 0.
      lv_error_num = lref_parser->num_errors( min_severity = if_ixml_parse_error=>co_fatal_error ).
      IF lv_error_num <> 0.
        DO lv_error_num TIMES.
          lv_index = sy-index - 1.

          CLEAR lv_str.
          lref_parse_error = lref_parser->get_error( index = lv_index ).
          lv_str = lref_parse_error->get_reason( ).

          WRITE / lv_str.
        ENDDO.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  cref_parser = lref_parser.
  cref_document = lref_document.

ENDFORM.                    " READ_XML

*&---------------------------------------------------------------------*
*&      Form  GET_XML_DATA
*&---------------------------------------------------------------------*
*       Parsing xml data
*----------------------------------------------------------------------*
*      -->IREF_PARSER     Parser XML
*      -->IREF_DOCUMENT   Document XML
*      <--CS_DATA         Data
*----------------------------------------------------------------------*
FORM get_xml_data USING iref_parser   TYPE REF TO if_ixml_parser
                        iref_document TYPE REF TO if_ixml_document
               CHANGING cv_msg_type   TYPE zedi_msg_type.

  DATA: lref_node     TYPE REF TO if_ixml_node,
        lref_iterator TYPE REF TO if_ixml_node_iterator,
        lref_nodemap  TYPE REF TO if_ixml_named_node_map,
        lref_attr     TYPE REF TO if_ixml_node,
        lv_name_elem  TYPE        string,
        lv_name_attr  TYPE        string,
        lv_prefix     TYPE        string,     "#ED NEEDED
        lv_value      TYPE        string,
        lv_count      TYPE        i,
        lv_index      TYPE        i.

* Process the document
  IF iref_parser->is_dom_generating( ) = 'X'.
    lref_node ?= iref_document.

    CHECK NOT lref_node IS INITIAL.
*   Create a node iterator
    lref_iterator = lref_node->create_iterator( ).
*   Get current node
    lref_node = lref_iterator->get_next( ).

*   Loop over all nodes
    WHILE NOT lref_node IS INITIAL.

      CASE lref_node->get_type( ).
        WHEN if_ixml_node=>co_node_element.
*         Element node
          lv_name_elem = lref_node->get_name( ).
          lv_value = lref_node->get_value( ).

          CASE lv_name_elem.
            WHEN 'MessageType'.
              cv_msg_type = lv_value.
            WHEN 'E0026'. " APERAK
              cv_msg_type = lv_value.
          ENDCASE.

          lref_nodemap = lref_node->get_attributes( ).
          IF NOT lref_nodemap IS INITIAL.
*           Attributes
            lv_count = lref_nodemap->get_length( ).
            DO lv_count TIMES.
              lv_index = sy-index - 1.

              lref_attr    = lref_nodemap->get_item( lv_index ).
              lv_name_attr = lref_attr->get_name( ).
              lv_prefix    = lref_attr->get_namespace_prefix( ).
              lv_value     = lref_attr->get_value( ).

            ENDDO.
          ENDIF.
        WHEN if_ixml_node=>co_node_text OR if_ixml_node=>co_node_cdata_section.
*         Text node
          lv_value = lref_node->get_value( ).
      ENDCASE.
*     Advance to next node
      lref_node = lref_iterator->get_next( ).
    ENDWHILE.

  ENDIF.

ENDFORM.                    " GET_XML_DATA

*&---------------------------------------------------------------------*
*&      Form  upload_data
*&---------------------------------------------------------------------*
FORM upload_data USING iv_file_path TYPE string
                       icl_msg TYPE REF TO zedi_cl_messages
              CHANGING cs_msg
                       cs_ret_default TYPE bapiret2.

  DATA: lv_xml_data  TYPE xstring,
        lv_file_path TYPE string.

  lv_file_path = iv_file_path.

  CASE abap_on.
    WHEN p_work.
      CALL METHOD zedi_cl_messages=>upload_msg_from_file
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
    WHEN p_serv.
      CALL METHOD zedi_cl_messages=>upload_msg_from_file
        EXPORTING
          iv_upload_type            = zedi_if_file_load_type=>server
          iv_file_path              = lv_file_path
        IMPORTING
          ev_file_data              = lv_xml_data
        EXCEPTIONS
          invalid_file_path         = 1
          upload_error              = 2
          upload_type_not_supported = 3
          OTHERS                    = 4.
  ENDCASE.

  IF lv_xml_data IS NOT INITIAL.
    CALL METHOD icl_msg->get_input_message
      EXPORTING
        iv_xml_data          = lv_xml_data
      IMPORTING
        es_msg_data          = cs_msg
      EXCEPTIONS
        proxy_fault          = 1
        transformation_error = 2
        critical_error       = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      "Ошибка получения данных файла!
      macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '065'.
    ENDIF.
  ENDIF.

ENDFORM.                    "upload_data

*&---------------------------------------------------------------------*
*&      Form  get_card_data
*&---------------------------------------------------------------------*
FORM get_card_data USING iv_msg_doc_id
                         iv_msg_receiver_id TYPE zedi_msg_receiver_id
                         iv_sender_gln      TYPE zedi_gln
                         iv_receiver_gln    TYPE zedi_gln
                         iv_msg_type        TYPE zedi_msg_type
                         iv_objtype         TYPE swo_objtyp
                         is_msg
                CHANGING cs_keyfields       TYPE /dfs/str_keyfields
                         cs_card_data
                         cv_status          TYPE /dfs/dokst
                         cs_ret_default     TYPE bapiret2.

  DATA: lv_bukrs TYPE bukrs.

  CLEAR: cs_keyfields.

* Найдем БЕ, которой адресовано сообщение
  CALL FUNCTION 'ZEDI_GET_BUKRS_DATA_BY_GLN'
    EXPORTING
      iv_gln         = iv_msg_receiver_id
    CHANGING
      cv_bukrs       = lv_bukrs
    EXCEPTIONS
      data_not_found = 1
      OTHERS         = 2.

  IF sy-subrc = 0.
    CASE iv_objtype.
      WHEN z030_if_030003_constants=>main_bo_name. "Электронная накладная
*       Найдем карточку, принадлежащую получателю сообщения
        SELECT SINGLE dokar doknr dokvr doktl FROM zdf_card_030003 INTO CORRESPONDING FIELDS OF cs_keyfields
          WHERE bukrs       = lv_bukrs
            AND delivery_id = iv_msg_doc_id.

        IF sy-subrc NE 0.
*         Это новый внешний документ. Создадим для него карточку.
          PERFORM create_dfs_card_030003_in USING lv_bukrs
                                                  iv_msg_type
                                                  is_msg
                                         CHANGING cs_keyfields
                                                  cs_ret_default.
        ENDIF.
      WHEN z030_if_030004_constants=>main_bo_name. "Акт выполненных работ
*       Найдем карточку, принадлежащую получателю сообщения
        SELECT SINGLE dokar doknr dokvr doktl FROM zdf_card_030004 INTO CORRESPONDING FIELDS OF cs_keyfields
          WHERE bukrs   = lv_bukrs
            AND bill_id = iv_msg_doc_id.

        IF sy-subrc NE 0.
*         Это новый внешний документ. Создадим для него карточку.
          PERFORM create_dfs_card_030004_in USING lv_bukrs
                                                  iv_msg_type
                                                  is_msg
                                         CHANGING cs_keyfields
                                                  cs_ret_default.
        ENDIF.
      WHEN z030_if_030005_constants=>main_bo_name. "Электронный документ
*       Найдем карточку, принадлежащую получателю сообщения
        SELECT SINGLE dokar doknr dokvr doktl FROM zdf_card_030005 INTO CORRESPONDING FIELDS OF cs_keyfields
          WHERE bukrs      = lv_bukrs
            AND sender_gln = iv_sender_gln
            AND recip_gln  = iv_receiver_gln
            AND doc_id     = iv_msg_doc_id.

        IF sy-subrc NE 0.
*         Это новый внешний документ. Создадим для него карточку.
          PERFORM create_dfs_card_030005_in USING lv_bukrs
                                                  iv_msg_type
                                                  is_msg
                                         CHANGING cs_keyfields
                                                  cs_ret_default.
        ENDIF.
    ENDCASE.
  ENDIF.

* Получим данные карточки
  IF cs_keyfields IS NOT INITIAL.
    CALL FUNCTION '/DFS/API_GET_CARD_INFO'
      EXPORTING
        keyfield             = cs_keyfields
      IMPORTING
        status               = cv_status
        data_card            = cs_card_data
      EXCEPTIONS
        card_not_found       = 1
        error_read_card_data = 2
        OTHERS               = 3.

    IF sy-subrc NE 0.
      " Ошибка получения данных карточки!
      macros_set_error cs_ret_default 'S' 'X' 'Z030_EDI' '010'.
    ENDIF.
  ENDIF.

  IF cs_keyfields IS INITIAL AND cs_ret_default IS INITIAL.
    " Карточка документа не найдена!
    macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '009'.
  ENDIF.

ENDFORM.                    "get_card_data

*&---------------------------------------------------------------------*
*&      Form  create_dfs_card_030003_in
*&---------------------------------------------------------------------*
FORM create_dfs_card_030003_in USING iv_bukrs     TYPE bukrs
                                     iv_msg_type  TYPE zedi_msg_type
                                     is_msg
                            CHANGING cs_keyfields   TYPE /dfs/str_keyfields
                                     cs_ret_default TYPE bapiret2.

  DATA: ls_card_data TYPE zdf_card_030003,
        lt_items     TYPE TABLE OF zdf_item_030003,
        ls_items     TYPE zdf_item_030003,
        lt_docum     TYPE TABLE OF zdf_docu_030003,
        ls_docum     TYPE zdf_docu_030003,
        lt_lgort     TYPE TABLE OF zedi_gln_lgort,
        ls_items_wbl TYPE zedi_wbl_line_item,
        ls_docum_wbl TYPE zedi_wbl_document,
        ls_items_dln TYPE zedi_dln_line_item,
        ls_docum_dln TYPE zedi_dln_document,
        lv_status    TYPE /dfs/dokst,
        lt_bapiret2  TYPE bapiret2 OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_wbl>       TYPE zedi_wbl,
                 <fs_dln>       TYPE zedi_dln,
                 <ft_items_wbl> TYPE table,
                 <ft_docum_wbl> TYPE table,
                 <ft_items_dln> TYPE table,
                 <ft_docum_dln> TYPE table.

  CASE iv_msg_type.
    WHEN 'BLRWBL'.
      ASSIGN is_msg TO <fs_wbl>.
      IF <fs_wbl> IS ASSIGNED.
        ls_card_data-invoice_type = '1'.           " Входящая
        ls_card_data-bukrs        = iv_bukrs.

        IF <fs_wbl>-message_header-test_indicator IS NOT INITIAL.
          ls_card_data-test_indicator = 'X'.
        ENDIF.

        ls_card_data-delivery_type   = <fs_wbl>-delivery_note-delivery_note_type.
        ls_card_data-delivery_id     = <fs_wbl>-delivery_note-delivery_note_id.
        ls_card_data-delivery_date   = <fs_wbl>-delivery_note-delivery_note_date.
        ls_card_data-contract_name   = <fs_wbl>-delivery_note-contract_name.
        ls_card_data-contract_id     = <fs_wbl>-delivery_note-contract_id.
        ls_card_data-contract_date   = <fs_wbl>-delivery_note-contract_date.
        ls_card_data-waybill_id      = <fs_wbl>-delivery_note-waybill_id.
        ls_card_data-quantity_trip   = <fs_wbl>-delivery_note-quantity_trip.
        ls_card_data-transport_owner = <fs_wbl>-delivery_note-transport_owner_name.
        ls_card_data-transport_id    = <fs_wbl>-delivery_note-transport_id.
        ls_card_data-trailer_id      = <fs_wbl>-delivery_note-trailer_id.
        ls_card_data-seal_id         = <fs_wbl>-delivery_note-seal_id.
        ls_card_data-order_id        = <fs_wbl>-delivery_note-order_id.
        ls_card_data-currency        = <fs_wbl>-delivery_note-currency.

        ls_card_data-shipper_gln     = <fs_wbl>-delivery_note-shipper-gln.
        ls_card_data-shipper_name    = <fs_wbl>-delivery_note-shipper-name.
        ls_card_data-shipper_address = <fs_wbl>-delivery_note-shipper-address.
        ls_card_data-shipper_contact = <fs_wbl>-delivery_note-shipper-contact.
        ls_card_data-shipper_vat_num = <fs_wbl>-delivery_note-shipper-vatregistration_number.

        ls_card_data-receiver_gln     = <fs_wbl>-delivery_note-receiver-gln.
        ls_card_data-receiver_name    = <fs_wbl>-delivery_note-receiver-name.
        ls_card_data-receiver_address = <fs_wbl>-delivery_note-receiver-address.
        ls_card_data-receiver_vat_num = <fs_wbl>-delivery_note-receiver-vatregistration_number.

        ls_card_data-fr_payer_gln     = <fs_wbl>-delivery_note-freight_payer-gln.
        ls_card_data-fr_payer_name    = <fs_wbl>-delivery_note-freight_payer-name.
        ls_card_data-fr_payer_address = <fs_wbl>-delivery_note-freight_payer-address.
        ls_card_data-fr_payer_vat_num = <fs_wbl>-delivery_note-freight_payer-vatregistration_number.

        ls_card_data-sh_from_gln     = <fs_wbl>-delivery_note-ship_from-gln.
        ls_card_data-sh_from_address = <fs_wbl>-delivery_note-ship_from-address.
        ls_card_data-sh_from_contact = <fs_wbl>-delivery_note-ship_from-contact.

        ls_card_data-sh_to_gln     = <fs_wbl>-delivery_note-ship_to-gln.
        ls_card_data-sh_to_address = <fs_wbl>-delivery_note-ship_to-address.

        ls_card_data-driver           = <fs_wbl>-delivery_note-carrier-transport_contact.
        ls_card_data-delivery_contact = <fs_wbl>-delivery_note-carrier-delivery_contact.
        ls_card_data-proxy_id         = <fs_wbl>-delivery_note-carrier-proxy_id.
        ls_card_data-proxy_date       = <fs_wbl>-delivery_note-carrier-proxy_date.
        ls_card_data-proxy_org_name   = <fs_wbl>-delivery_note-carrier-party_issuing_proxy_name.

        CASE ls_card_data-fr_payer_gln.
          WHEN ls_card_data-shipper_gln.  ls_card_data-x_fp_shipper  = abap_on.
          WHEN ls_card_data-receiver_gln. ls_card_data-x_fp_receiver = abap_on.
        ENDCASE.

*       Таблица товарных позиций
        ASSIGN <fs_wbl>-delivery_note-despatch_advice_logistic_unit-line_item TO <ft_items_wbl>.
        IF <ft_items_wbl> IS ASSIGNED.
          LOOP AT <ft_items_wbl> INTO ls_items_wbl.
            CLEAR: ls_items.

            ls_items-num              = ls_items_wbl-line_item_number.
            ls_items-sign             = ls_items_wbl-line_item_sign.
            ls_items-id               = ls_items_wbl-line_item_id.
            ls_items-buyer_id         = ls_items_wbl-line_item_buyer_id.
            ls_items-custom_code      = ls_items_wbl-item_custom_code.
            ls_items-dn_type_prev     = ls_items_wbl-delivery_type_prev.
            ls_items-dn_prev          = ls_items_wbl-delivery_note_prev.
            ls_items-dn_date_prev     = ls_items_wbl-delivery_note_date_prev.
            ls_items-dn_prev_line_id  = ls_items_wbl-delivery_note_prev_line_id.
            ls_items-supplier_id      = ls_items_wbl-line_item_supplier_id.
            ls_items-name             = ls_items_wbl-line_item_name.
            ls_items-gross_weight_val = ls_items_wbl-gross_weight_value.
            ls_items-quan_despatched  = ls_items_wbl-quantity_despatched.
            ls_items-quan_uom         = ls_items_wbl-line_item_quantity_uom.
            ls_items-quan_unit        = ls_items_wbl-despatch_unit_quantity_despatc.
            ls_items-country_origin   = ls_items_wbl-country_of_origin.
            ls_items-best_before_date = ls_items_wbl-best_before_date.
            ls_items-quan_despat_spt  = ls_items_wbl-quantity_despatched_spt.
            ls_items-quan_spt         = ls_items_wbl-line_item_quantity_spt.
            ls_items-tax_rate         = ls_items_wbl-tax_rate.
            ls_items-additional_info  = ls_items_wbl-additional_information.
            ls_items-amount_wo_charge = ls_items_wbl-line_item_amount_without_charg.
            ls_items-amount_charge    = ls_items_wbl-line_item_amount_charges.
            ls_items-amount           = ls_items_wbl-line_item_amount.
            ls_items-price            = ls_items_wbl-line_item_price.

            APPEND ls_items TO lt_items.
          ENDLOOP.
        ENDIF.

*       Таблица документов
        ASSIGN <fs_wbl>-delivery_note-document TO <ft_docum_wbl>.
        IF <ft_docum_wbl> IS ASSIGNED.
          LOOP AT <ft_docum_wbl> INTO ls_docum_wbl.
            ls_docum-doc_id   = ls_docum_wbl-document_id.
            ls_docum-doc_name = ls_docum_wbl-document_name.
            ls_docum-doc_date = ls_docum_wbl-document_date.
            APPEND ls_docum TO lt_docum.
          ENDLOOP.
        ENDIF.

      ENDIF.
    WHEN 'BLRDLN'.
      ASSIGN is_msg TO <fs_dln>.
      IF <fs_dln> IS ASSIGNED.
        ls_card_data-invoice_type = '1'.           " Входящая
        ls_card_data-bukrs        = iv_bukrs.

        IF <fs_dln>-message_header-test_indicator IS NOT INITIAL.
          ls_card_data-test_indicator = 'X'.
        ENDIF.

        ls_card_data-delivery_type = <fs_dln>-delivery_note-delivery_note_type.
        ls_card_data-delivery_id   = <fs_dln>-delivery_note-delivery_note_id.
        ls_card_data-delivery_date = <fs_dln>-delivery_note-delivery_note_date.
        ls_card_data-contract_name = <fs_dln>-delivery_note-contract_name.
        ls_card_data-contract_id   = <fs_dln>-delivery_note-contract_id.
        ls_card_data-contract_date = <fs_dln>-delivery_note-contract_date.
        ls_card_data-order_id      = <fs_dln>-delivery_note-order_id.
        ls_card_data-currency      = <fs_dln>-delivery_note-currency.

        ls_card_data-shipper_gln     = <fs_dln>-delivery_note-shipper-gln.
        ls_card_data-shipper_name    = <fs_dln>-delivery_note-shipper-name.
        ls_card_data-shipper_address = <fs_dln>-delivery_note-shipper-address.
        ls_card_data-shipper_contact = <fs_dln>-delivery_note-shipper-contact.
        ls_card_data-shipper_vat_num = <fs_dln>-delivery_note-shipper-vatregistration_number.

        ls_card_data-receiver_gln     = <fs_dln>-delivery_note-receiver-gln.
        ls_card_data-receiver_name    = <fs_dln>-delivery_note-receiver-name.
        ls_card_data-receiver_address = <fs_dln>-delivery_note-receiver-address.
        ls_card_data-receiver_vat_num = <fs_dln>-delivery_note-receiver-vatregistration_number.

        ls_card_data-sh_from_gln     = <fs_dln>-delivery_note-ship_from-gln.
        ls_card_data-sh_from_address = <fs_dln>-delivery_note-ship_from-address.
        ls_card_data-sh_from_contact = <fs_dln>-delivery_note-ship_from-contact.

        ls_card_data-sh_to_gln     = <fs_dln>-delivery_note-ship_to-gln.
        ls_card_data-sh_to_address = <fs_dln>-delivery_note-ship_to-address.

        ls_card_data-delivery_contact = <fs_dln>-delivery_note-carrier-delivery_contact.
        ls_card_data-proxy_id         = <fs_dln>-delivery_note-carrier-proxy_id.
        ls_card_data-proxy_org_name   = <fs_dln>-delivery_note-carrier-party_issuing_proxy_name.
        ls_card_data-proxy_date       = <fs_dln>-delivery_note-carrier-proxy_date.

*       Таблица товарных позиций
        ASSIGN <fs_dln>-delivery_note-despatch_advice_logistic_unit-line_item TO <ft_items_dln>.
        IF <ft_items_dln> IS ASSIGNED.
          LOOP AT <ft_items_dln> INTO ls_items_dln.
            CLEAR: ls_items.

            ls_items-num              = ls_items_dln-line_item_number.
            ls_items-sign             = ls_items_dln-line_item_sign.
            ls_items-id               = ls_items_dln-line_item_id.
            ls_items-buyer_id         = ls_items_dln-line_item_buyer_id.
            ls_items-custom_code      = ls_items_dln-item_custom_code.
            ls_items-dn_type_prev     = ls_items_dln-delivery_type_prev.
            ls_items-dn_prev          = ls_items_dln-delivery_note_prev.
            ls_items-dn_date_prev     = ls_items_dln-delivery_note_date_prev.
            ls_items-dn_prev_line_id  = ls_items_dln-delivery_note_prev_line_id.
            ls_items-supplier_id      = ls_items_dln-line_item_supplier_id.
            ls_items-name             = ls_items_dln-line_item_name.
            ls_items-quan_despatched  = ls_items_dln-quantity_despatched.
            ls_items-quan_uom         = ls_items_dln-line_item_quantity_uom.
            ls_items-country_origin   = ls_items_dln-country_of_origin.
            ls_items-best_before_date = ls_items_dln-best_before_date.
            ls_items-quan_despat_spt  = ls_items_dln-quantity_despatched_spt.
            ls_items-quan_spt         = ls_items_dln-line_item_quantity_spt.
            ls_items-tax_rate         = ls_items_dln-tax_rate.
            ls_items-additional_info  = ls_items_dln-additional_information.
            ls_items-amount_wo_charge = ls_items_dln-line_item_amount_without_charg.
            ls_items-amount_charge    = ls_items_dln-line_item_amount_charges.
            ls_items-amount           = ls_items_dln-line_item_amount.
            ls_items-price            = ls_items_dln-line_item_price.

            APPEND ls_items TO lt_items.
          ENDLOOP.
        ENDIF.

*       Таблица документов
        ASSIGN <fs_dln>-delivery_note-document TO <ft_docum_dln>.
        IF <ft_docum_dln> IS ASSIGNED.
          LOOP AT <ft_docum_dln> INTO ls_docum_dln.
            ls_docum-doc_id   = ls_docum_dln-document_id.
            ls_docum-doc_name = ls_docum_dln-document_name.
            ls_docum-doc_date = ls_docum_dln-document_date.
            APPEND ls_docum TO lt_docum.
          ENDLOOP.
        ENDIF.

      ENDIF.
    WHEN OTHERS.
      EXIT. " Выход, если сообщение иного типа
  ENDCASE.

* Статус
  lv_status = 'EA'.

* Завод/склад пункта погрузки
  REFRESH: lt_lgort.
  CALL FUNCTION 'ZEDI_GET_LGORT_DATA_BY_GLN'
    EXPORTING
      iv_gln         = ls_card_data-sh_from_gln
    TABLES
      ct_lgort       = lt_lgort
    CHANGING
      cv_werks       = ls_card_data-sh_from_werks
      cv_lgort       = ls_card_data-sh_from_lgort
    EXCEPTIONS
      data_not_found = 1
      OTHERS         = 2.
  IF sy-subrc = 0.
    " Не заполняем завод/склад, если их несколько на один GLN
    IF lines( lt_lgort ) > 1.
      CLEAR: ls_card_data-sh_from_werks,
             ls_card_data-sh_from_lgort.
    ENDIF.
  ENDIF.

* Завод/склад пункта разгрузки
  REFRESH: lt_lgort.
  CALL FUNCTION 'ZEDI_GET_LGORT_DATA_BY_GLN'
    EXPORTING
      iv_gln         = ls_card_data-sh_to_gln
    TABLES
      ct_lgort       = lt_lgort
    CHANGING
      cv_werks       = ls_card_data-sh_to_werks
      cv_lgort       = ls_card_data-sh_to_lgort
      cv_contact     = ls_card_data-sh_to_contact
    EXCEPTIONS
      data_not_found = 1
      OTHERS         = 2.
  IF sy-subrc = 0.
    " Не заполняем завод/склад, если их несколько на один GLN
    IF lines( lt_lgort ) > 1.
      CLEAR: ls_card_data-sh_to_werks,
             ls_card_data-sh_to_lgort,
             ls_card_data-sh_to_contact.
    ENDIF.
  ENDIF.

* Данные бизнес-партнера
  CALL FUNCTION 'ZEDI_GET_PARTNER_DATA_BY_GLN'
    EXPORTING
      iv_gln         = ls_card_data-shipper_gln
    CHANGING
      cv_lifnr       = ls_card_data-lifnr
    EXCEPTIONS
      data_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    " Отсутствуют настройки для бизнес-партнера!
    macros_set_error_param cs_ret_default 'E' 'X' 'Z030_EDI' '050' ls_card_data-shipper_name
                                                                   ls_card_data-shipper_gln
                                                                   ls_card_data-shipper_vat_num ''.
  ENDIF.

* Создадим карточку
  CALL FUNCTION 'ZDF_API_CREATE_CARD_030003'
    EXPORTING
      mode                   = 'CREATE'
      commit                 = 'X'
    TABLES
      return                 = lt_bapiret2
      documents              = lt_docum
      items                  = lt_items
    CHANGING
      data_card              = ls_card_data
      keyfield               = cs_keyfields
      status                 = lv_status
    EXCEPTIONS
      card_not_found         = 1
      table_not_found        = 2
      data_not_found         = 3
      authority_check_error  = 4
      create_dms_error       = 5
      insert_data_error      = 6
      insert_link_error      = 7
      keys_not_found         = 8
      document_target_exist  = 9
      select_data_error      = 10
      document_not_exist     = 11
      break_enhancement      = 12
      application_error      = 13
      critical_error         = 14
      source_data_error      = 15
      attach_file_error      = 16
      create_file_link_error = 17
      description_not_found  = 18
      link_error             = 19
      status_error           = 20
      OTHERS                 = 21.

  IF sy-subrc <> 0.
    " Ошибка создания карточки накладной!
    macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '005'.
  ENDIF.

ENDFORM.                    "create_dfs_card_030003_in

*&---------------------------------------------------------------------*
*&      Form  create_dfs_card_030004_in
*&---------------------------------------------------------------------*
FORM create_dfs_card_030004_in USING iv_bukrs       TYPE bukrs
                                     iv_msg_type    TYPE zedi_msg_type
                                     is_msg
                            CHANGING cs_keyfields   TYPE /dfs/str_keyfields
                                     cs_ret_default TYPE bapiret2.

  DATA: ls_card_data TYPE zdf_card_030004,
        lt_items     TYPE TABLE OF zdf_item_030004,
        ls_items     TYPE zdf_item_030004,
        lt_lgort     TYPE TABLE OF zedi_gln_lgort,
        ls_items_inv TYPE zedi_inv_line_item,
        lv_status    TYPE /dfs/dokst,
        lt_bapiret2  TYPE bapiret2 OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_inv>         TYPE zedi_inv,
                 <ft_proxy_items> TYPE table.

  CASE iv_msg_type.
    WHEN 'BLRINV'.
      ASSIGN is_msg TO <fs_inv>.
      IF <fs_inv> IS ASSIGNED.

        ls_card_data-doc_type         = '1'. " Входящий
        ls_card_data-bill_type        = <fs_inv>-invoice-invoice_type.
        ls_card_data-bukrs            = iv_bukrs.

        IF <fs_inv>-message_header-test_indicator IS NOT INITIAL.
          ls_card_data-test_indicator = 'X'.
        ENDIF.

        ls_card_data-bill_id          = <fs_inv>-invoice-invoice_id.
        ls_card_data-bill_date        = <fs_inv>-invoice-invoice_date.
        ls_card_data-contract_id      = <fs_inv>-invoice-contract_id.
        ls_card_data-contract_date    = <fs_inv>-invoice-contract_date.
        ls_card_data-delivery_id      = <fs_inv>-invoice-delivery_note_id.
        ls_card_data-delivery_date    = <fs_inv>-invoice-delivery_note_date.
        ls_card_data-ref_bill_id      = <fs_inv>-invoice-reference_invoice_id.
        ls_card_data-ref_bill_date    = <fs_inv>-invoice-reference_invoice_date.
        ls_card_data-order_id         = <fs_inv>-invoice-order_id.
        ls_card_data-currency         = <fs_inv>-invoice-currency.

        ls_card_data-suppl_gln        = <fs_inv>-invoice-supplier-gln.
        ls_card_data-suppl_name       = <fs_inv>-invoice-supplier-name.
        ls_card_data-suppl_address    = <fs_inv>-invoice-supplier-address.
        ls_card_data-suppl_vat_num    = <fs_inv>-invoice-supplier-vatregistration_number.
        ls_card_data-suppl_acc_num    = <fs_inv>-invoice-supplier-account_number.
        ls_card_data-suppl_bank_name  = <fs_inv>-invoice-supplier-bank_name.
        ls_card_data-suppl_bank_addr  = <fs_inv>-invoice-supplier-bank_address.
        ls_card_data-suppl_bank_code  = <fs_inv>-invoice-supplier-bank_code.

        ls_card_data-buyer_gln        = <fs_inv>-invoice-buyer-gln.
        ls_card_data-buyer_name       = <fs_inv>-invoice-buyer-name.
        ls_card_data-buyer_address    = <fs_inv>-invoice-buyer-address.
        ls_card_data-buyer_vat_num    = <fs_inv>-invoice-buyer-vatregistration_number.
        ls_card_data-buyer_acc_num    = <fs_inv>-invoice-buyer-account_number.
        ls_card_data-buyer_bank_name  = <fs_inv>-invoice-buyer-bank_name.
        ls_card_data-buyer_bank_addr  = <fs_inv>-invoice-buyer-bank_address.
        ls_card_data-buyer_bank_code  = <fs_inv>-invoice-buyer-bank_code.

        ls_card_data-shipper_gln      = <fs_inv>-invoice-shipper-gln.
        ls_card_data-shipper_name     = <fs_inv>-invoice-shipper-name.
        ls_card_data-shipper_address  = <fs_inv>-invoice-shipper-address.
        ls_card_data-shipper_vat_num  = <fs_inv>-invoice-shipper-vatregistration_number.

        ls_card_data-receiver_gln     = <fs_inv>-invoice-receiver-gln.
        ls_card_data-receiver_name    = <fs_inv>-invoice-receiver-name.
        ls_card_data-receiver_address = <fs_inv>-invoice-receiver-address.
        ls_card_data-receiver_vat_num = <fs_inv>-invoice-receiver-vatregistration_number.

        ls_card_data-sh_from_gln      = <fs_inv>-invoice-ship_from-gln.
        ls_card_data-sh_from_address  = <fs_inv>-invoice-ship_from-address.

        ls_card_data-sh_to_gln        = <fs_inv>-invoice-ship_to-gln.
        ls_card_data-sh_to_address    = <fs_inv>-invoice-ship_to-address.

*       Таблица товарных позиций
        ASSIGN <fs_inv>-invoice-invoice_line_item-line_item TO <ft_proxy_items>.
        IF <ft_proxy_items> IS ASSIGNED.
          LOOP AT <ft_proxy_items> INTO ls_items_inv.
            CLEAR: ls_items.

            ls_items-num              = ls_items_inv-line_item_number.
            ls_items-id               = ls_items_inv-line_item_id.
            ls_items-buyer_id         = ls_items_inv-line_item_buyer_id.
            ls_items-supplier_id      = ls_items_inv-line_item_supplier_id.
            ls_items-name             = ls_items_inv-line_item_name.
            ls_items-quan_despatched  = ls_items_inv-quantity.
            ls_items-quan_uom         = ls_items_inv-line_item_quantity_uom.
            ls_items-country_origin   = ls_items_inv-country_of_origin.
            ls_items-tax_rate         = ls_items_inv-tax_rate.
            ls_items-additional_info  = ls_items_inv-additional_information.
            ls_items-amount_wo_charge = ls_items_inv-line_item_amount_without_charg.
            ls_items-amount_excise    = ls_items_inv-line_item_amount_excise.
            ls_items-amount_charge    = ls_items_inv-line_item_amount_charges.
            ls_items-amount           = ls_items_inv-line_item_amount.
            ls_items-price            = ls_items_inv-line_item_price.

            APPEND ls_items TO lt_items.
          ENDLOOP.
        ENDIF.

      ENDIF.
    WHEN OTHERS.
      EXIT. " Выход, если сообщение иного типа
  ENDCASE.

* Статус
  lv_status = 'EA'.

* Завод/склад пункта погрузки
  REFRESH: lt_lgort.
  CALL FUNCTION 'ZEDI_GET_LGORT_DATA_BY_GLN'
    EXPORTING
      iv_gln         = ls_card_data-sh_from_gln
    TABLES
      ct_lgort       = lt_lgort
    CHANGING
      cv_werks       = ls_card_data-sh_from_werks
      cv_lgort       = ls_card_data-sh_from_lgort
    EXCEPTIONS
      data_not_found = 1
      OTHERS         = 2.
  IF sy-subrc = 0.
    " Не заполняем завод/склад, если их несколько на один GLN
    IF lines( lt_lgort ) > 1.
      CLEAR: ls_card_data-sh_from_werks,
             ls_card_data-sh_from_lgort.
    ENDIF.
  ENDIF.

* Завод/склад пункта разгрузки
  REFRESH: lt_lgort.
  CALL FUNCTION 'ZEDI_GET_LGORT_DATA_BY_GLN'
    EXPORTING
      iv_gln         = ls_card_data-sh_to_gln
    TABLES
      ct_lgort       = lt_lgort
    CHANGING
      cv_werks       = ls_card_data-sh_to_werks
      cv_lgort       = ls_card_data-sh_to_lgort
    EXCEPTIONS
      data_not_found = 1
      OTHERS         = 2.
  IF sy-subrc = 0.
    " Не заполняем завод/склад, если их несколько на один GLN
    IF lines( lt_lgort ) > 1.
      CLEAR: ls_card_data-sh_to_werks,
             ls_card_data-sh_to_lgort.
    ENDIF.
  ENDIF.

* Данные бизнес-партнера
  CALL FUNCTION 'ZEDI_GET_PARTNER_DATA_BY_GLN'
    EXPORTING
      iv_gln         = ls_card_data-suppl_gln
    CHANGING
      cv_lifnr       = ls_card_data-lifnr
    EXCEPTIONS
      data_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    " Отсутствуют настройки для бизнес-партнера!
    macros_set_error_param cs_ret_default 'E' 'X' 'Z030_EDI' '050' ls_card_data-suppl_name
                                                                   ls_card_data-suppl_gln
                                                                   ls_card_data-suppl_vat_num ''.
  ENDIF.

* Создадим карточку
  CALL FUNCTION 'ZDF_API_CREATE_CARD_030004'
    EXPORTING
      mode                   = 'CREATE'
      commit                 = 'X'
    TABLES
      return                 = lt_bapiret2
      items_einv             = lt_items
    CHANGING
      data_card              = ls_card_data
      keyfield               = cs_keyfields
      status                 = lv_status
    EXCEPTIONS
      card_not_found         = 1
      table_not_found        = 2
      data_not_found         = 3
      authority_check_error  = 4
      create_dms_error       = 5
      insert_data_error      = 6
      insert_link_error      = 7
      keys_not_found         = 8
      document_target_exist  = 9
      select_data_error      = 10
      document_not_exist     = 11
      break_enhancement      = 12
      application_error      = 13
      critical_error         = 14
      source_data_error      = 15
      attach_file_error      = 16
      create_file_link_error = 17
      description_not_found  = 18
      link_error             = 19
      status_error           = 20
      OTHERS                 = 21.

  IF sy-subrc <> 0.
    " Ошибка создания карточки документа!
    macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '005'.
  ENDIF.

ENDFORM.                    "create_dfs_card_030004_in

*&---------------------------------------------------------------------*
*&      Form  create_dfs_card_030005_in
*&---------------------------------------------------------------------*
FORM create_dfs_card_030005_in USING iv_bukrs       TYPE bukrs
                                     iv_msg_type    TYPE zedi_msg_type
                                     is_msg
                            CHANGING cs_keyfields   TYPE /dfs/str_keyfields
                                     cs_ret_default TYPE bapiret2.

  DATA: ls_card_data TYPE zdf_card_030005,
        lt_refd      TYPE TABLE OF zdf_refd_030005,
        ls_refd      TYPE zdf_refd_030005,
        ls_refd_doc  TYPE zedi_doc_reference_document,
        lv_status    TYPE /dfs/dokst,
        lt_bapiret2  TYPE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: lt_content_binary TYPE sdokcntbin OCCURS 0 WITH HEADER LINE,
        ls_rcrd_data      TYPE /dfs/rcrd_record_data,
        lv_objkey         TYPE swo_typeid,
        lv_file_name      TYPE string,
        lv_file_size      TYPE sdok_fsize,
        lv_length         TYPE i,
        lv_xstr           TYPE xstring,
        lv_smb.

  FIELD-SYMBOLS: <fs_doc>        TYPE zedi_doc,
                 <ft_proxy_refd> TYPE table.

  CASE iv_msg_type.
    WHEN 'BLRDOC'.
      ASSIGN is_msg TO <fs_doc>.
      IF <fs_doc> IS ASSIGNED.

        ls_card_data-doc_type         = '1'. " Входящий
        ls_card_data-bukrs            = iv_bukrs.

        IF <fs_doc>-message_header-test_indicator IS NOT INITIAL.
          ls_card_data-test_indicator = 'X'.
        ENDIF.

        ls_card_data-doc_id           = <fs_doc>-document-document_id.
        ls_card_data-doc_date         = <fs_doc>-document-creation_date_time(8).
        ls_card_data-edoc_type        = <fs_doc>-document-e_document_type.
        ls_card_data-edoc_number      = <fs_doc>-document-e_document_number.
        ls_card_data-edoc_date        = <fs_doc>-document-e_document_date.

        ls_card_data-edoc_orig_number = <fs_doc>-document-e_document_original_number.
        ls_card_data-edoc_orig_date   = <fs_doc>-document-e_document_original_date.

        ls_card_data-sender_gln       = <fs_doc>-document-sender-gln.
        ls_card_data-sender_name      = <fs_doc>-document-sender-name.
        ls_card_data-sender_address   = <fs_doc>-document-sender-address.
        ls_card_data-sender_vat_num   = <fs_doc>-document-sender-vatregistration_number.

        ls_card_data-recip_gln        = <fs_doc>-document-receiver-gln.
        ls_card_data-recip_name       = <fs_doc>-document-receiver-name.
        ls_card_data-recip_address    = <fs_doc>-document-receiver-address.
        ls_card_data-recip_vat_num    = <fs_doc>-document-receiver-vatregistration_number.

*       Таблица ссылочных документов
        ASSIGN <fs_doc>-document-reference_document TO <ft_proxy_refd>.
        IF <ft_proxy_refd> IS ASSIGNED.
          LOOP AT <ft_proxy_refd> INTO ls_refd_doc.
            CLEAR: ls_refd.

            ls_refd-refdoc_type   = ls_refd_doc-type.
            ls_refd-refdoc_name   = ls_refd_doc-name.
            ls_refd-refdoc_number = ls_refd_doc-number.
            ls_refd-refdoc_date   = ls_refd_doc-date.

            APPEND ls_refd TO lt_refd.
          ENDLOOP.
        ENDIF.

      ENDIF.
    WHEN OTHERS.
      EXIT. " Выход, если сообщение иного типа
  ENDCASE.

* Статус
  lv_status = 'EA'.

* Данные бизнес-партнера
  CALL FUNCTION 'ZEDI_GET_PARTNER_DATA_BY_GLN'
    EXPORTING
      iv_gln         = ls_card_data-sender_gln
    CHANGING
      cv_lifnr       = ls_card_data-lifnr
    EXCEPTIONS
      data_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    " Отсутствуют настройки для бизнес-партнера!
    macros_set_error_param cs_ret_default 'E' 'X' 'Z030_EDI' '050' ls_card_data-sender_name
                                                                   ls_card_data-sender_gln
                                                                   ls_card_data-sender_vat_num ''.
  ENDIF.

* Создадим карточку
  CALL FUNCTION 'ZDF_API_CREATE_CARD_030005'
    EXPORTING
      mode                   = 'CREATE'
      commit                 = 'X'
    TABLES
      return                 = lt_bapiret2
      refdocs                = lt_refd
    CHANGING
      data_card              = ls_card_data
      keyfield               = cs_keyfields
      status                 = lv_status
    EXCEPTIONS
      card_not_found         = 1
      table_not_found        = 2
      data_not_found         = 3
      authority_check_error  = 4
      create_dms_error       = 5
      insert_data_error      = 6
      insert_link_error      = 7
      keys_not_found         = 8
      document_target_exist  = 9
      select_data_error      = 10
      document_not_exist     = 11
      break_enhancement      = 12
      application_error      = 13
      critical_error         = 14
      source_data_error      = 15
      attach_file_error      = 16
      create_file_link_error = 17
      description_not_found  = 18
      link_error             = 19
      status_error           = 20
      OTHERS                 = 21.

  IF sy-subrc <> 0.
    " Ошибка создания карточки документа!
    macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '005'.
  ELSE.
*   Прикрепим файл документа
    lv_file_name = <fs_doc>-document-e_document_name.

    ls_rcrd_data-description = lv_file_name.
    ls_rcrd_data-doclocation = 'R'.
    ls_rcrd_data-doctype     = 'SCN'.
    ls_rcrd_data-objtype     = z030_if_030005_constants=>main_bo_name.

    CALL FUNCTION '/DFS/GET_CARD_STRKEY'
      EXPORTING
        dokar  = cs_keyfields-dokar
        doknr  = cs_keyfields-doknr
        dokvr  = cs_keyfields-dokvr
        doktl  = cs_keyfields-doktl
      IMPORTING
        strkey = lv_objkey.

    " Расширение файла
    lv_length = strlen( lv_file_name ).
    WHILE lv_length > 0.
      lv_length = lv_length - 1.
      lv_smb = lv_file_name+lv_length(1).

      IF lv_smb = '.'.
        lv_length = lv_length + 1.
        ls_rcrd_data-dappl = lv_file_name+lv_length.
        EXIT.
      ENDIF.
    ENDWHILE.

    "Содержимое файла
    lv_xstr = <fs_doc>-document-e_document.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstr
      IMPORTING
        output_length = lv_length
      TABLES
        binary_tab    = lt_content_binary[].

    "Загрузка файла
    lv_file_size = lv_length.
    CALL FUNCTION '/DFS/RCRD_ATTACHFILE2BO'
      EXPORTING
        i_objtype            = z030_if_030005_constants=>main_bo_name
        i_objkey             = lv_objkey
        i_file_size          = lv_file_size
      TABLES
        ct_content_binary    = lt_content_binary[]
      CHANGING
        cs_rcrd_data         = ls_rcrd_data
      EXCEPTIONS
        e_create_record      = 1
        e_create_bo2rcrd_rel = 2
        OTHERS               = 3.

    IF sy-subrc <> 0.
      " Ошибка прикрепления сообщения к карточке!
      macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '002'.
    ENDIF.

*** Создание соединений с другими документами
    PERFORM create_doc_link TABLES lt_refd
                             USING ls_card_data.

  ENDIF.

ENDFORM.                    "create_dfs_card_030005_in

*&---------------------------------------------------------------------*
*&      Form  create_doc_link
*&---------------------------------------------------------------------*
FORM create_doc_link TABLES ct_refd STRUCTURE zdf_refd_030005
                      USING is_card_data TYPE zdf_card_030005.

  DATA: ls_refd          TYPE zdf_refd_030005,
        ls_keyfields_doc TYPE /dfs/str_keyfields,
        ls_keyfields_ref TYPE /dfs/str_keyfields,
        lv_cardnum_doc   TYPE /dfs/cardnum,
        lv_cardnum_ref   TYPE /dfs/cardnum,
        lv_objkey_doc    TYPE swo_objid,
        lv_objkey_ref    TYPE swo_objid,
        lv_objtyp_doc    TYPE swo_objtyp,
        lv_objtyp_ref    TYPE swo_objtyp,
        lv_msg_type      TYPE zedi_msg_type.

  MOVE-CORRESPONDING: is_card_data TO ls_keyfields_doc.

  CALL FUNCTION '/DFS/GET_CARD_STRKEY'
    EXPORTING
      dokar  = is_card_data-dokar
      doknr  = is_card_data-doknr
      dokvr  = is_card_data-dokvr
      doktl  = is_card_data-doktl
    IMPORTING
      strkey = lv_objkey_doc.

  lv_cardnum_doc = z030_if_030005_constants=>card_number.
  lv_objtyp_doc  = z030_if_030005_constants=>main_bo_name.

  LOOP AT ct_refd INTO ls_refd.
    CLEAR: ls_keyfields_ref.

    lv_msg_type = ls_refd-refdoc_type.

*   Определяем бизнес-объект карточки сообщения
    CALL METHOD zedi_cl_messages=>get_card_objtype
      EXPORTING
        iv_msg_type         = lv_msg_type
      IMPORTING
        ev_objtype          = lv_objtyp_ref
        ev_cardnum          = lv_cardnum_ref
      EXCEPTIONS
        objtype_not_defined = 1
        OTHERS              = 2.

    IF sy-subrc = 0.
      CASE lv_msg_type.
        WHEN 'BLRWBL' OR 'BLRDLN'.
          SELECT SINGLE dokar doknr dokvr doktl FROM zdf_card_030003 INTO CORRESPONDING FIELDS OF ls_keyfields_ref
            WHERE bukrs       = is_card_data-bukrs
              AND delivery_id = ls_refd-refdoc_number.
        WHEN 'BLRINV'.
          SELECT SINGLE dokar doknr dokvr doktl FROM zdf_card_030004 INTO CORRESPONDING FIELDS OF ls_keyfields_ref
            WHERE bukrs   = is_card_data-bukrs
              AND bill_id = ls_refd-refdoc_number.
        WHEN 'BLRDOC'.
          SELECT SINGLE dokar doknr dokvr doktl FROM zdf_card_030005 INTO CORRESPONDING FIELDS OF ls_keyfields_ref
            WHERE bukrs      = is_card_data-bukrs
              AND sender_gln = is_card_data-sender_gln
              AND recip_gln  = is_card_data-recip_gln
              AND doc_id     = ls_refd-refdoc_number.
      ENDCASE.

*     Создаем соединения на документы
      IF ls_keyfields_ref IS NOT INITIAL.
        CALL FUNCTION '/DFS/GET_CARD_STRKEY'
          EXPORTING
            dokar  = ls_keyfields_ref-dokar
            doknr  = ls_keyfields_ref-doknr
            dokvr  = ls_keyfields_ref-dokvr
            doktl  = ls_keyfields_ref-doktl
          IMPORTING
            strkey = lv_objkey_ref.

        CALL FUNCTION '/DFS/LINK_CREATE'
          EXPORTING
            i_card            = lv_cardnum_doc
            is_keyfield       = ls_keyfields_doc
            i_objkey          = lv_objkey_ref
            i_objtype         = lv_objtyp_ref
            ix_message        = ' '
            ix_log            = 'X'
          EXCEPTIONS
            botype_not_exists = 1
            cust_read_error   = 2
            cust_error        = 3
            link_error        = 4
            OTHERS            = 5.

        CALL FUNCTION '/DFS/LINK_CREATE'
          EXPORTING
            i_card            = lv_cardnum_ref
            is_keyfield       = ls_keyfields_ref
            i_objkey          = lv_objkey_doc
            i_objtype         = lv_objtyp_doc
            ix_message        = ' '
            ix_log            = 'X'
          EXCEPTIONS
            botype_not_exists = 1
            cust_read_error   = 2
            cust_error        = 3
            link_error        = 4
            OTHERS            = 5.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zbt_stop_all_tasks
*&---------------------------------------------------------------------*
FORM zbt_stop_all_tasks USING is_keyfields  TYPE /dfs/str_keyfields
                              is_rcrd_data  TYPE /dfs/rcrd_record_data
                     CHANGING cv_status     TYPE /dfs/dokst.

  DATA: lt_apprdata    TYPE TABLE OF /dfs/apprdata,
        ls_ret_default TYPE bapiret2.

  IF is_rcrd_data-docstatus = '1'. "Отмена
*   Получим данные маршрута
    SELECT * FROM /dfs/apprdata INTO TABLE lt_apprdata
      WHERE dokar = is_keyfields-dokar
        AND doknr = is_keyfields-doknr
        AND dokvr = is_keyfields-dokvr
        AND doktl = is_keyfields-doktl
        AND aprst IN ('S','W').
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    CALL FUNCTION '/DFS/MAINTAIN_CARD'
      EXPORTING
        mode              = 3
        s_keyfield        = is_keyfields
        runtype           = 998
        okcode            = '/DFS/BT_STOP_ALL'
        ix_catch_messages = 'X'
      IMPORTING
        es_return         = ls_ret_default
      EXCEPTIONS
        card_not_found    = 1
        OTHERS            = 2.
    IF sy-subrc = 0.
*     Если остановка прошла успешно, нужно перечитать статус карточки
      CALL FUNCTION '/DFS/API_GET_CARD_INFO'
        EXPORTING
          keyfield             = is_keyfields
        IMPORTING
          status               = cv_status
        EXCEPTIONS
          card_not_found       = 1
          error_read_card_data = 2
          OTHERS               = 3.
    ENDIF.

  ENDIF.

ENDFORM.                    "zbt_stop_all_tasks

*&---------------------------------------------------------------------*
*&      Form  change_dfs_card_in
*&---------------------------------------------------------------------*
FORM change_dfs_card_in USING iv_msg_type TYPE zedi_msg_type
                              iv_objtype  TYPE swo_objtyp
                              is_msg
                     CHANGING cs_card_data
                              cs_ret_default TYPE bapiret2.

  DATA: lv_card       TYPE /dfs/cardnum,
        ls_keyfields  TYPE /dfs/str_keyfields,
        lt_report     TYPE TABLE OF zdf_repo_030003,
        ls_report     TYPE zdf_repo_030003,
        ls_report_wbr TYPE zedi_wbr_report,
        lt_bapiret2   TYPE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: lt_descr_alv TYPE TABLE OF type_controls_table_enh WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_030003>     TYPE zdf_card_030003,
                 <fs_wbr>        TYPE zedi_wbr,
                 <fs_dnr>        TYPE zedi_dnr,
                 <fs_t_report>   TYPE table,
                 <ft_report_wbr> TYPE table.

  MOVE-CORRESPONDING: cs_card_data TO ls_keyfields.

* Получим данные ALV-таблиц карточки
  CALL FUNCTION '/DFS/API_GET_CARD_INFO'
    EXPORTING
      keyfield             = ls_keyfields
    TABLES
      descr_alv            = lt_descr_alv
    EXCEPTIONS
      card_not_found       = 1
      error_read_card_data = 2
      OTHERS               = 3.
  IF sy-subrc NE 0.
    " Ошибка получения данных карточки!
    macros_set_error cs_ret_default 'S' 'X' 'Z030_EDI' '010'.
  ENDIF.

  CASE iv_msg_type.
    WHEN 'BLRWBR'.
      ASSIGN cs_card_data TO <fs_030003>.
      IF <fs_030003> IS ASSIGNED.
        " Актуально только для исходящей накладной
        IF <fs_030003>-invoice_type <> '2'. " Исходящая
          EXIT.
        ENDIF.

        ASSIGN is_msg TO <fs_wbr>.
        IF <fs_wbr> IS ASSIGNED.

          <fs_030003>-seal_id_receiver = <fs_wbr>-delivery_note-seal_idreceiver.
          <fs_030003>-sh_to_contact    = <fs_wbr>-delivery_note-ship_to-contact.

*         Таблица актов
          ASSIGN <fs_wbr>-delivery_note-report TO <ft_report_wbr>.
          IF <ft_report_wbr> IS ASSIGNED.
            READ TABLE lt_descr_alv WITH KEY fieldname = 'REPORTS'.
            IF sy-subrc = 0.
*             Получаем ссылку на таблицу актов
              ASSIGN lt_descr_alv-ref_tab->* TO <fs_t_report>.
              IF sy-subrc = 0.

                LOOP AT <ft_report_wbr> INTO ls_report_wbr.
                  ls_report-report_id   = ls_report_wbr-report_id.
                  ls_report-report_name = ls_report_wbr-report_name.
                  ls_report-report_date = ls_report_wbr-report_date.
                  APPEND ls_report TO <fs_t_report>.
                ENDLOOP.

              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
    WHEN 'BLRDNR'.
      ASSIGN cs_card_data TO <fs_030003>.
      IF <fs_030003> IS ASSIGNED.
        " Актуально только для исходящей накладной
        IF <fs_030003>-invoice_type <> '2'. " Исходящая
          EXIT.
        ENDIF.

        ASSIGN is_msg TO <fs_dnr>.
        IF <fs_dnr> IS ASSIGNED.

          <fs_030003>-sh_to_contact = <fs_dnr>-delivery_note-ship_to-contact.

        ENDIF.
      ENDIF.
    WHEN OTHERS.
      EXIT. " Выход, если сообщение иного типа
  ENDCASE.

  lv_card = iv_objtype+4.
  CALL FUNCTION '/DFS/API_CHANGE_CARD'
    EXPORTING
      card                   = lv_card
      keyfield               = ls_keyfields
    TABLES
      return                 = lt_bapiret2
      descr_alv              = lt_descr_alv
    CHANGING
      data_card              = cs_card_data
    EXCEPTIONS
      card_not_found         = 1
      table_not_found        = 2
      data_not_found         = 3
      keys_not_found         = 4
      authority_check_error  = 5
      update_card_error      = 6
      document_not_exist     = 7
      select_card_error      = 8
      dms_document_locked    = 9
      lock_card_error        = 10
      change_dms_error       = 11
      insert_originals_error = 12
      break_enhancement      = 13
      critical_error         = 14
      application_error      = 15
      alv_data_error         = 16
      link_error             = 17
      source_data_error      = 18
      OTHERS                 = 19.

  IF sy-subrc <> 0.
    " Ошибка обновления данных карточки!
    macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '043'.
  ENDIF.

ENDFORM.                    "change_dfs_card_in

*&---------------------------------------------------------------------*
*&      Form  get_routing_data
*&---------------------------------------------------------------------*
FORM get_routing_data USING iv_objtype     TYPE swo_objtyp
                            is_rcrd_data   TYPE /dfs/rcrd_record_data
                            iv_status      TYPE /dfs/dokst
                   CHANGING cs_msg_routing TYPE zedi_msg_routing
                            cs_ret_default TYPE bapiret2.
  DATA: lv_msg1 TYPE symsgv,
        lv_msg2 TYPE symsgv,
        lv_msg3 TYPE symsgv,
        lv_msg4 TYPE symsgv.

* Получим данные маршрутизации
  SELECT SINGLE * FROM zedi_msg_routing INTO cs_msg_routing
    WHERE objtype    = iv_objtype
      AND route      = 'I'
      AND doctype    = is_rcrd_data-doctype
      AND docstatus  = is_rcrd_data-docstatus
      AND doccomment = is_rcrd_data-comment
      AND status     = iv_status.

  IF sy-subrc NE 0.
    CONCATENATE iv_objtype 'I'
                is_rcrd_data-doctype
                is_rcrd_data-docstatus
                is_rcrd_data-comment
                iv_status INTO lv_msg1 SEPARATED BY space.

    " Ошибка чтения данных маршрутизации!
    macros_set_error_param cs_ret_default 'E' 'X' 'Z030_EDI' '022' lv_msg1 lv_msg2 lv_msg3 lv_msg4.
  ENDIF.

ENDFORM.                    "get_routing_data

*&---------------------------------------------------------------------*
*&      Form  attach_file_to_card
*&---------------------------------------------------------------------*
FORM attach_file_to_card USING iv_objtype     TYPE swo_objtyp
                               is_keyfields   TYPE /dfs/str_keyfields
                               iv_file_path   TYPE string
                               iv_file_name   TYPE string
                               is_msg_routing TYPE zedi_msg_routing
                               iv_description
                      CHANGING cs_rcrd_data   TYPE /dfs/rcrd_record_data
                               cs_ret_default TYPE bapiret2.

  DATA: lt_content_binary TYPE sdokcntbin OCCURS 0 WITH HEADER LINE,
        lv_objkey         TYPE swo_typeid,
        lv_file_size      TYPE sdok_fsize,
        lv_str_length     TYPE i.

  cs_rcrd_data-description = iv_file_name.
  cs_rcrd_data-doclocation = is_msg_routing-doclocation.
  cs_rcrd_data-objtype     = iv_objtype.

  CALL FUNCTION '/DFS/GET_CARD_STRKEY'
    EXPORTING
      dokar  = is_keyfields-dokar
      doknr  = is_keyfields-doknr
      dokvr  = is_keyfields-dokvr
      doktl  = is_keyfields-doktl
    IMPORTING
      strkey = lv_objkey.

  CASE abap_on.
    WHEN p_work.
      cs_rcrd_data-filename = iv_file_path.

      CALL FUNCTION '/DFS/RCRD_ATTACHFILE2BO'
        EXPORTING
          i_objtype            = iv_objtype
          i_objkey             = lv_objkey
        CHANGING
          cs_rcrd_data         = cs_rcrd_data
        EXCEPTIONS
          e_create_record      = 1
          e_create_bo2rcrd_rel = 2
          OTHERS               = 3.

      IF sy-subrc <> 0.
        " Ошибка прикрепления сообщения к карточке!
        macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '002'.
      ENDIF.
    WHEN p_serv.
      OPEN DATASET iv_file_path FOR INPUT IN BINARY MODE.

      DO.
        READ DATASET iv_file_path INTO lt_content_binary-line ACTUAL LENGTH lv_str_length.
        IF lv_str_length > 0.
          APPEND lt_content_binary.
          lv_file_size = lv_file_size + lv_str_length.
        ELSE.
          CLOSE DATASET iv_file_path.
          EXIT.
        ENDIF.
      ENDDO.

*     Заполним расширение файла
      lv_str_length = strlen( iv_file_path ).
      lv_str_length = lv_str_length - 3.
      cs_rcrd_data-dappl = iv_file_path+lv_str_length.

      CALL FUNCTION '/DFS/RCRD_ATTACHFILE2BO'
        EXPORTING
          i_objtype            = iv_objtype
          i_objkey             = lv_objkey
          i_file_size          = lv_file_size
        TABLES
          ct_content_binary    = lt_content_binary[]
        CHANGING
          cs_rcrd_data         = cs_rcrd_data
        EXCEPTIONS
          e_create_record      = 1
          e_create_bo2rcrd_rel = 2
          OTHERS               = 3.

      IF sy-subrc <> 0.
        " Ошибка прикрепления сообщения к карточке!
        macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '002'.
      ENDIF.
  ENDCASE.

*** Добавим примечание в карточку
  PERFORM add_routing_comment_to_card USING is_keyfields
                                            cs_rcrd_data
                                            is_msg_routing
                                   CHANGING cs_ret_default.

*** Добавим описание статуса в "Примечание"
  IF cs_rcrd_data-doctype = 'APN' AND ( cs_rcrd_data-comment = '2750' OR " Текст запроса на корректировку
                                        cs_rcrd_data-comment = '2570' OR " Текст ошибки верификации накладной
                                        cs_rcrd_data-comment = '2571' ). " Текст ошибки верификации накладной

    PERFORM add_comment_to_card USING is_keyfields
                                      iv_description
                             CHANGING cs_ret_default.
  ENDIF.


ENDFORM.                    "attach_file_to_card

*&---------------------------------------------------------------------*
*&      Form  file_processing
*&---------------------------------------------------------------------*
FORM file_processing USING iv_msg_type  TYPE zedi_msg_type
                           iv_file_path TYPE string
                           iv_file_name TYPE string
                           icl_msg TYPE REF TO zedi_cl_messages
                  CHANGING cs_msg
                           cs_ret_default TYPE bapiret2.

  DATA: ls_card_data   TYPE REF TO data,
        ls_rcrd_data   TYPE /dfs/rcrd_record_data,
        ls_keyfields   TYPE /dfs/str_keyfields,
        ls_msg_routing TYPE zedi_msg_routing.

  DATA: lv_status_new        TYPE /dfs/dokst,
        lv_status_old        TYPE /dfs/dokst,
        lv_msg_doc_id(70)    TYPE c,
        lv_msg_receiver_id   TYPE zedi_msg_receiver_id,
        lv_sender_gln        TYPE zedi_gln,
        lv_receiver_gln      TYPE zedi_gln,
        lv_objtype           TYPE swo_objtyp,
        lv_tab_type          TYPE typename,
        lv_description(2560) TYPE c.

  FIELD-SYMBOLS: <fs_card_data> TYPE any.

*** Загрузим входящий файл
  PERFORM upload_data USING iv_file_path icl_msg
                   CHANGING cs_msg
                            cs_ret_default.
  IF cs_ret_default-type CA 'EA'.
    EXIT.
  ENDIF.

*** Получим ключевые параметры сообщения
  PERFORM get_msg_key_parameters USING iv_msg_type
                                       cs_msg
                              CHANGING lv_msg_doc_id
                                       lv_msg_receiver_id
                                       lv_sender_gln
                                       lv_receiver_gln
                                       lv_objtype
                                       ls_rcrd_data
                                       lv_description
                                       cs_ret_default.
  IF cs_ret_default-type CA 'EA'.
    EXIT.
  ENDIF.

*** Создадим тип данных карточки
  lv_tab_type = lv_objtype.
  REPLACE FIRST OCCURRENCE OF 'ZDF_' IN lv_tab_type WITH 'ZDF_CARD_'.
  CREATE DATA ls_card_data TYPE (lv_tab_type).
  IF ls_card_data IS BOUND.
    ASSIGN ls_card_data->* TO <fs_card_data>.
  ENDIF.

*** Получим данные карточки
  PERFORM get_card_data USING lv_msg_doc_id
                              lv_msg_receiver_id
                              lv_sender_gln
                              lv_receiver_gln
                              iv_msg_type
                              lv_objtype
                              cs_msg
                     CHANGING ls_keyfields
                              <fs_card_data>
                              lv_status_old
                              cs_ret_default.

  IF cs_ret_default-type CA 'EA'.
    EXIT.
  ENDIF.

*** Остановим поток операций, если пришел запрос на отмену
  PERFORM zbt_stop_all_tasks USING ls_keyfields
                                   ls_rcrd_data
                          CHANGING lv_status_old.

*** Если нужно, обновим данные карточки из входящего сообщения
  PERFORM change_dfs_card_in USING iv_msg_type
                                   lv_objtype
                                   cs_msg
                          CHANGING <fs_card_data>
                                   cs_ret_default.

  IF cs_ret_default-type CA 'EA'.
    EXIT.
  ENDIF.

*** Определим новый статус карточки
  PERFORM get_doc_status_new USING lv_objtype
                                   <fs_card_data>
                                   ls_rcrd_data
                                   lv_status_old
                          CHANGING lv_status_new
                                   cs_ret_default.

  IF cs_ret_default-type CA 'EA'.
    EXIT.
  ENDIF.

*** Получим данные маршрутизации
  PERFORM get_routing_data USING lv_objtype
                                 ls_rcrd_data
                                 lv_status_new
                        CHANGING ls_msg_routing
                                 cs_ret_default.

  IF cs_ret_default-type CA 'EA'.
    EXIT.
  ENDIF.

*** Прикрепим файл к карточке
  PERFORM attach_file_to_card USING lv_objtype
                                    ls_keyfields
                                    iv_file_path
                                    iv_file_name
                                    ls_msg_routing
                                    lv_description
                           CHANGING ls_rcrd_data
                                    cs_ret_default.

  IF cs_ret_default-type CA 'EA'.
    EXIT.
  ENDIF.

*** Если нужно перевести карточку в иной статус, то сделаем это
  IF lv_status_new <> lv_status_old.
    PERFORM change_doc_status USING ls_keyfields
                                    lv_status_new
                           CHANGING cs_ret_default.

    IF cs_ret_default-type CA 'EA'.
      EXIT.
    ENDIF.
  ENDIF.

*** Отправим сообщение
  PERFORM send_notification USING lv_objtype
                                  ls_keyfields
                                  <fs_card_data>
                                  ls_msg_routing
                                  lv_description.

*** Обработаем зависимые карточки
  PERFORM dependent_doc_processing USING lv_objtype
                                         ls_keyfields
                                         <fs_card_data>
                                         lv_status_new
                                         ls_rcrd_data.

ENDFORM.                    "file_processing

*&---------------------------------------------------------------------*
*&      Form  get_msg_key_parameters
*&---------------------------------------------------------------------*
FORM get_msg_key_parameters USING iv_msg_type TYPE zedi_msg_type
                                  is_msg
                         CHANGING cv_msg_doc_id
                                  cv_msg_receiver_id TYPE zedi_msg_receiver_id
                                  cv_sender_gln      TYPE zedi_gln
                                  cv_receiver_gln    TYPE zedi_gln
                                  cv_objtype         TYPE swo_objtyp
                                  cs_rcrd_data       TYPE /dfs/rcrd_record_data
                                  cv_description
                                  cs_ret_default     TYPE bapiret2.

  DATA: lv_msg_id      TYPE zedi_msg_id,
        lv_msg_type    TYPE zedi_msg_type,
        lv_refdoc_type TYPE zedi_msg_type.

  FIELD-SYMBOLS: <lv_cmp> TYPE any.

  DEFINE macros_get_component.
    ASSIGN COMPONENT &1 OF STRUCTURE is_msg TO <lv_cmp>.
    IF <lv_cmp> IS ASSIGNED.
      &2 = <lv_cmp>.
    ENDIF.
  END-OF-DEFINITION.

  macros_get_component 'MESSAGE_HEADER-MESSAGE_TYPE'    lv_msg_type.
  macros_get_component 'MESSAGE_HEADER-MESSAGE_ID'      lv_msg_id.
  macros_get_component 'MESSAGE_HEADER-MSG_RECEIVER_ID' cv_msg_receiver_id.

  cs_rcrd_data-doctype = lv_msg_type+3.

  CASE iv_msg_type.
    WHEN 'BLRWBL' OR 'BLRWBR' OR 'BLRDLN' OR 'BLRDNR'.
      macros_get_component 'DELIVERY_NOTE-DELIVERY_NOTE_ID' cv_msg_doc_id.
      macros_get_component 'DELIVERY_NOTE-FUNCTION_CODE'    cs_rcrd_data-docstatus.
      lv_refdoc_type = iv_msg_type.
    WHEN 'BLRINV'.
      macros_get_component 'INVOICE-INVOICE_ID'    cv_msg_doc_id.
      macros_get_component 'INVOICE-FUNCTION_CODE' cs_rcrd_data-docstatus.
      lv_refdoc_type = iv_msg_type.
    WHEN 'BLRDOC'.
      macros_get_component 'DOCUMENT-DOCUMENT_ID'   cv_msg_doc_id.
      macros_get_component 'DOCUMENT-FUNCTION_CODE' cs_rcrd_data-docstatus.
      macros_get_component 'DOCUMENT-SENDER-GLN'    cv_sender_gln.
      macros_get_component 'DOCUMENT-RECEIVER-GLN'  cv_receiver_gln.
      lv_refdoc_type = iv_msg_type.
    WHEN 'BLRAPN'.
      macros_get_component 'ACKNOWLEDGEMENT-DELIVERY_NOTE_ID'                     cv_msg_doc_id.
      macros_get_component 'ACKNOWLEDGEMENT-FUNCTION_CODE'                        cs_rcrd_data-docstatus.
      macros_get_component 'ACKNOWLEDGEMENT-ERROR_OR_ACKNOWLEDGEMENT-CODE'        cs_rcrd_data-comment.
      macros_get_component 'ACKNOWLEDGEMENT-ERROR_OR_ACKNOWLEDGEMENT-DESCRIPTION' cv_description.
      macros_get_component 'ACKNOWLEDGEMENT-REFERENCE_DOCUMENT-TYPE'              lv_refdoc_type.

      " Случай, когда и исходное, и ссылочное сообщение - BLRAPN
      IF lv_refdoc_type = iv_msg_type.
        lv_refdoc_type = lv_msg_id(6).
        IF cs_rcrd_data-comment = '2651'.
          lv_refdoc_type = 'BLRWBL'.
        ENDIF.
      ENDIF.

      IF lv_refdoc_type = 'BLRDOC'.
        macros_get_component 'ACKNOWLEDGEMENT-REFERENCE_DOCUMENT-ID' cv_msg_doc_id.
        macros_get_component 'ACKNOWLEDGEMENT-SHIPPER-GLN'           cv_sender_gln.
        macros_get_component 'ACKNOWLEDGEMENT-RECEIVER-GLN'          cv_receiver_gln.
      ENDIF.
  ENDCASE.

* Определяем бизнес-объект карточки сообщения
  CALL METHOD zedi_cl_messages=>get_card_objtype
    EXPORTING
      iv_msg_type         = lv_refdoc_type
    IMPORTING
      ev_objtype          = cv_objtype
    EXCEPTIONS
      objtype_not_defined = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
    " Тип бизнес-объекта не определен!
    macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '060'.
  ENDIF.

  IF cv_msg_doc_id IS INITIAL.
    " Номер документа не определен!
    macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '008'.
  ENDIF.

  IF cv_msg_receiver_id IS INITIAL.
    " Получатель сообщения не определен!
    macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '042'.
  ENDIF.

ENDFORM.                    "get_msg_key_parameters

*&---------------------------------------------------------------------*
*&      Form  get_doc_status_new
*&---------------------------------------------------------------------*
FORM get_doc_status_new USING iv_objtype     TYPE swo_objtyp
                              is_card_data
                              is_rcrd_data   TYPE /dfs/rcrd_record_data
                              iv_status_old  TYPE /dfs/dokst
                     CHANGING cv_status_new  TYPE /dfs/dokst
                              cs_ret_default TYPE bapiret2.

  DATA: lv_route_type TYPE zedi_route_type.

  FIELD-SYMBOLS: <lv_cmp> TYPE any.

  DEFINE macros_get_component.
    ASSIGN COMPONENT &1 OF STRUCTURE is_card_data TO <lv_cmp>.
    IF <lv_cmp> IS ASSIGNED.
      &2 = <lv_cmp>.
    ENDIF.
  END-OF-DEFINITION.

  CASE iv_objtype.
    WHEN z030_if_030003_constants=>main_bo_name. macros_get_component 'INVOICE_TYPE' lv_route_type.
    WHEN z030_if_030004_constants=>main_bo_name. macros_get_component 'DOC_TYPE'     lv_route_type.
    WHEN z030_if_030005_constants=>main_bo_name. macros_get_component 'DOC_TYPE'     lv_route_type.
  ENDCASE.

  SELECT SINGLE status_new FROM zedi_msg_status INTO cv_status_new
    WHERE objtype    = iv_objtype
      AND route      = 'I'
      AND route_type = lv_route_type
      AND doctype    = is_rcrd_data-doctype
      AND docstatus  = is_rcrd_data-docstatus
      AND doccomment = is_rcrd_data-comment
      AND status_old = iv_status_old.

  IF sy-subrc NE 0.
    cv_status_new = iv_status_old.
  ENDIF.

ENDFORM.                    "get_doc_status_new

*&---------------------------------------------------------------------*
*&      Form  change_doc_status
*&---------------------------------------------------------------------*
FORM change_doc_status USING is_keyfields   TYPE /dfs/str_keyfields
                             iv_status      TYPE /dfs/dokst
                    CHANGING cs_ret_default TYPE bapiret2.

  CALL FUNCTION '/DFS/CHANGE_DOC_STATUS3'
    EXPORTING
      is_keyfields = is_keyfields
      i_status_new = iv_status
      i_commit     = 'X'
    IMPORTING
      es_return    = cs_ret_default.

  IF cs_ret_default-type CA 'EA'.
    " Ошибка перевода карточки в новый статус!
    macros_set_error cs_ret_default 'E' 'X' 'Z030_EDI' '023'.
  ENDIF.

ENDFORM.                    "change_doc_status

*&---------------------------------------------------------------------*
*&      Form  add_routing_comment_to_card
*&---------------------------------------------------------------------*
FORM add_routing_comment_to_card USING is_keyfields   TYPE /dfs/str_keyfields
                                       is_rcrd_data   TYPE /dfs/rcrd_record_data
                                       is_msg_routing TYPE zedi_msg_routing
                              CHANGING cs_ret_default TYPE bapiret2.

  DATA: lt_lines TYPE TABLE OF tline,
        ls_lines TYPE tline.

* Сформируем текст сообщения
  IF is_msg_routing-msg_text IS NOT INITIAL.
    ls_lines-tdline = is_msg_routing-msg_text.
    APPEND ls_lines TO lt_lines.
  ENDIF.

  CONCATENATE 'Файл сообщения:' is_rcrd_data-description INTO ls_lines-tdline SEPARATED BY space.
  APPEND ls_lines TO lt_lines.

  CALL FUNCTION '/DFS/SAVE_TEXT'
    EXPORTING
      keyfield        = is_keyfields
      fieldname       = 'CARD_COMMENT'
    TABLES
      lines           = lt_lines
    EXCEPTIONS
      save_text_error = 1
      OTHERS          = 99.

  IF sy-subrc NE 0.
    " Ошибка записи в журнал!
    macros_set_error cs_ret_default 'S' '' 'Z030_EDI' '006'.
  ENDIF.

ENDFORM.                    "add_routing_comment_to_card

*&---------------------------------------------------------------------*
*&      Form  add_comment_to_card
*&---------------------------------------------------------------------*
FORM add_comment_to_card USING is_keyfields   TYPE /dfs/str_keyfields
                               iv_description
                      CHANGING cs_ret_default TYPE bapiret2.

  DATA: lt_lines TYPE TABLE OF tline,
        ls_lines TYPE tline.

  DATA: lv_text(4096) TYPE c,
        lv_line(132)  TYPE c,
        lt_wrap       TYPE TABLE OF string.

  lv_text = iv_description.

  CALL FUNCTION 'IQAPI_WORD_WRAP'
    EXPORTING
      textline            = lv_text
      outputlen           = 132
    TABLES
      out_lines           = lt_wrap
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.

* Сформируем текст сообщения
  LOOP AT lt_wrap INTO lv_line.
    ls_lines-tdline = lv_line.
    APPEND ls_lines TO lt_lines.
  ENDLOOP.

  IF lt_lines IS INITIAL.
    EXIT.
  ENDIF.

  CALL FUNCTION '/DFS/SAVE_TEXT'
    EXPORTING
      keyfield        = is_keyfields
      fieldname       = 'CARD_COMMENT'
    TABLES
      lines           = lt_lines
    EXCEPTIONS
      save_text_error = 1
      OTHERS          = 99.

  IF sy-subrc NE 0.
    " Ошибка записи в журнал!
    macros_set_error cs_ret_default 'S' 'X' 'Z030_EDI' '006'.
  ENDIF.

ENDFORM.                    "add_comment_to_card

*&---------------------------------------------------------------------*
*&      Form  send_notification
*&---------------------------------------------------------------------*
FORM send_notification USING iv_objtype     TYPE swo_objtyp
                             is_keyfields   TYPE /dfs/str_keyfields
                             is_card_data
                             is_msg_routing TYPE zedi_msg_routing
                             iv_description.

  DATA: ls_apprdata      TYPE /dfs/apprdata,
        lt_apprdata      TYPE TABLE OF /dfs/apprdata,
        lt_user_list     TYPE bapibname      OCCURS 0 WITH HEADER LINE,
        lt_gln_users     TYPE zedi_gln_users OCCURS 0 WITH HEADER LINE,
        lv_gln           TYPE zedi_gln,
        lv_line          TYPE so_text255,
        lv_str1          TYPE string,
        lv_str2          TYPE string,
        lv_str3          TYPE string,
        lv_tabix         TYPE sy-tabix,
        lv_bp_number(10) TYPE c,
        lv_date_str(10)  TYPE c,
        lv_langu         TYPE c.

  DATA: ls_items_030003 TYPE zdf_item_030003,
        lt_items_030003 TYPE TABLE OF zdf_item_030003,
        ls_items_030004 TYPE zdf_item_030004,
        lt_items_030004 TYPE TABLE OF zdf_item_030004.

* Define default values and parameters for FM '/DFS/SEND_LETTER'
  DATA: lf_sap_mail(1)     VALUE '',
        lf_ext_mail(1)     VALUE 'X',
        lv_msg_subject(50) VALUE 'Уведомление о получении электронного сообщения',
        lv_link_descr(50)  VALUE 'Карточка электронной накладной'.

  DATA: lv_link_runtype TYPE /dfs/runtype,
        lv_link_mode    TYPE i,
        lt_msg_content  LIKE solisti1 OCCURS 0 WITH HEADER LINE.

  DATA: lt_wrap TYPE TABLE OF string.

  FIELD-SYMBOLS: <fs_030003> TYPE zdf_card_030003,
                 <fs_030004> TYPE zdf_card_030004,
                 <fs_030005> TYPE zdf_card_030005.

  IF is_msg_routing-msg_send IS INITIAL.
    EXIT.
  ENDIF.

  CASE iv_objtype.
    WHEN z030_if_030003_constants=>main_bo_name. " ---------------------------------------- Электронная накладная ----------------------------------------
      ASSIGN is_card_data TO <fs_030003>.

*     Выбираем данные по карточке
      CALL FUNCTION 'ZDF_API_GET_CARD_INFO_030003'
        EXPORTING
          keyfield             = is_keyfields
        TABLES
          items                = lt_items_030003
        EXCEPTIONS
          card_not_found       = 1
          error_read_card_data = 2
          break_enhancement    = 3
          critical_error       = 4
          application_error    = 5
          OTHERS               = 6.

*     Дата накладной
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          date_internal = <fs_030003>-delivery_date
        IMPORTING
          date_external = lv_date_str.

*     Текст:
      APPEND is_msg_routing-msg_text TO lt_msg_content.
      APPEND INITIAL LINE TO lt_msg_content.

      CONCATENATE 'Номер накладной:' <fs_030003>-delivery_id 'от' lv_date_str INTO lv_line SEPARATED BY space.
      CONCATENATE lv_line '.' INTO lv_line.
      APPEND lv_line TO lt_msg_content.
      APPEND INITIAL LINE TO lt_msg_content.

*     Контрагент
      CLEAR: lv_line.
      CASE <fs_030003>-invoice_type.
        WHEN '1'. "Входящая
          CONCATENATE 'Контрагент:' <fs_030003>-lifnr <fs_030003>-shipper_name  INTO lv_line SEPARATED BY space.
          APPEND lv_line TO lt_msg_content.
          APPEND INITIAL LINE TO lt_msg_content.
        WHEN '2'. "Исходящая
          CONCATENATE 'Контрагент:' <fs_030003>-kunnr <fs_030003>-receiver_name INTO lv_line SEPARATED BY space.
          APPEND lv_line TO lt_msg_content.
          APPEND INITIAL LINE TO lt_msg_content.
      ENDCASE.

      CLEAR: lv_line.
      IF <fs_030003>-sh_from_werks IS NOT INITIAL AND <fs_030003>-sh_from_lgort IS NOT INITIAL.
        CONCATENATE <fs_030003>-sh_from_werks '/' <fs_030003>-sh_from_lgort INTO lv_line.
      ENDIF.
      CONCATENATE 'Пункт погрузки:' lv_line <fs_030003>-sh_from_address INTO lv_line SEPARATED BY space.
      CONCATENATE lv_line '.' INTO lv_line.
      APPEND lv_line TO lt_msg_content.

      CLEAR: lv_line.
      IF <fs_030003>-sh_to_werks IS NOT INITIAL AND <fs_030003>-sh_to_lgort IS NOT INITIAL.
        CONCATENATE <fs_030003>-sh_to_werks '/' <fs_030003>-sh_to_lgort INTO lv_line.
      ENDIF.
      CONCATENATE 'Пункт разгрузки:' lv_line <fs_030003>-sh_to_address INTO lv_line SEPARATED BY space.
      CONCATENATE lv_line '.' INTO lv_line.
      APPEND lv_line TO lt_msg_content.
      APPEND INITIAL LINE TO lt_msg_content.

*     Товарные позиции (первые 5 строк)
      SORT lt_items_030003 BY num.

      LOOP AT lt_items_030003 INTO ls_items_030003.
        lv_tabix = sy-tabix.

        lv_str1 = ls_items_030003-num.
        SHIFT lv_str1 LEFT DELETING LEADING '0'.

        CONCATENATE lv_str1 ls_items_030003-name INTO lv_line SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
        APPEND lv_line TO lt_msg_content.

        IF lv_tabix = 5.
          lv_line = '...'.
          APPEND lv_line TO lt_msg_content.
          EXIT.
        ENDIF.
      ENDLOOP.
      APPEND INITIAL LINE TO lt_msg_content.

      IF iv_description IS NOT INITIAL.
        CALL FUNCTION 'IQAPI_WORD_WRAP'
          EXPORTING
            textline            = iv_description
            outputlen           = 245
          TABLES
            out_lines           = lt_wrap
          EXCEPTIONS
            outputlen_too_large = 1
            OTHERS              = 2.

        IF sy-subrc = 0.
          LOOP AT lt_wrap INTO lv_line.
            IF sy-tabix = 1.
              CONCATENATE 'Описание:' lv_line INTO lv_line SEPARATED BY space.
            ENDIF.
            APPEND lv_line TO lt_msg_content.
          ENDLOOP.
        ENDIF.
      ENDIF.

*     Наименование ссылки на карточку
      lv_link_descr = <fs_030003>-delivery_id.

*     Определим необходимый GLN для поиска получателей сообщения
      CASE is_msg_routing-runtype.
        WHEN '3'.                           " Накладная исходящая
          lv_gln = <fs_030003>-sh_from_gln.
        WHEN '4'.                           " Накладная входящая
          lv_gln = <fs_030003>-sh_to_gln.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

    WHEN z030_if_030004_constants=>main_bo_name. " ---------------------------------------- Акт выполненных работ ----------------------------------------
      ASSIGN is_card_data TO <fs_030004>.

*     Выбираем данные по карточке
      CALL FUNCTION 'ZDF_API_GET_CARD_INFO_030004'
        EXPORTING
          keyfield             = is_keyfields
        TABLES
          items_einv           = lt_items_030004
        EXCEPTIONS
          card_not_found       = 1
          error_read_card_data = 2
          break_enhancement    = 3
          critical_error       = 4
          application_error    = 5
          OTHERS               = 6.

*     Дата акта
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          date_internal = <fs_030004>-bill_date
        IMPORTING
          date_external = lv_date_str.

*     Текст:
      APPEND is_msg_routing-msg_text TO lt_msg_content.
      APPEND INITIAL LINE TO lt_msg_content.

      CONCATENATE 'Номер акта:' <fs_030004>-bill_id 'от' lv_date_str INTO lv_line SEPARATED BY space.
      CONCATENATE lv_line '.' INTO lv_line.
      APPEND lv_line TO lt_msg_content.
      APPEND INITIAL LINE TO lt_msg_content.

*     Контрагент
      CASE <fs_030004>-doc_type.
        WHEN '1'. "Входящий
          CONCATENATE 'Контрагент:' <fs_030004>-lifnr <fs_030004>-suppl_name INTO lv_line SEPARATED BY space.
          APPEND lv_line TO lt_msg_content.
          APPEND INITIAL LINE TO lt_msg_content.
        WHEN '2'. "Исходящий
          CONCATENATE 'Контрагент:' <fs_030004>-kunnr <fs_030004>-buyer_name INTO lv_line SEPARATED BY space.
          APPEND lv_line TO lt_msg_content.
          APPEND INITIAL LINE TO lt_msg_content.
      ENDCASE.

*     Товарные позиции (первые 5 строк)
      SORT lt_items_030004 BY num.

      LOOP AT lt_items_030004 INTO ls_items_030004.
        lv_tabix = sy-tabix.

        lv_str1 = ls_items_030004-num.
        SHIFT lv_str1 LEFT DELETING LEADING '0'.

        CONCATENATE lv_str1 ls_items_030004-name INTO lv_line SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
        APPEND lv_line TO lt_msg_content.

        IF lv_tabix = 5.
          lv_line = '...'.
          APPEND lv_line TO lt_msg_content.
          EXIT.
        ENDIF.
      ENDLOOP.
      APPEND INITIAL LINE TO lt_msg_content.

*     Описание
      IF iv_description IS NOT INITIAL.
        CALL FUNCTION 'IQAPI_WORD_WRAP'
          EXPORTING
            textline            = iv_description
            outputlen           = 245
          TABLES
            out_lines           = lt_wrap
          EXCEPTIONS
            outputlen_too_large = 1
            OTHERS              = 2.

        IF sy-subrc = 0.
          LOOP AT lt_wrap INTO lv_line.
            IF sy-tabix = 1.
              CONCATENATE 'Описание:' lv_line INTO lv_line SEPARATED BY space.
            ENDIF.
            APPEND lv_line TO lt_msg_content.
          ENDLOOP.
        ENDIF.
      ENDIF.

*     Наименование ссылки на карточку
      lv_link_descr = <fs_030004>-bill_id.

*     Определим необходимый GLN для поиска получателей сообщения
      CASE is_msg_routing-runtype.
        WHEN '3'.                           " Документ исходящий
          lv_gln = <fs_030004>-suppl_gln.
        WHEN '4'.                           " Документ входящий
          lv_gln = <fs_030004>-buyer_gln.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

    WHEN z030_if_030005_constants=>main_bo_name. " ---------------------------------------- Электронный документ ----------------------------------------
      ASSIGN is_card_data TO <fs_030005>.

*     Дата документа
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          date_internal = <fs_030005>-doc_date
        IMPORTING
          date_external = lv_date_str.

*     Текст:
      APPEND is_msg_routing-msg_text TO lt_msg_content.
      APPEND INITIAL LINE TO lt_msg_content.

      CONCATENATE 'Номер документа:' <fs_030005>-doc_id 'от' lv_date_str INTO lv_line SEPARATED BY space.
      CONCATENATE lv_line '.' INTO lv_line.
      APPEND lv_line TO lt_msg_content.
      APPEND INITIAL LINE TO lt_msg_content.

*     Контрагент
      IF <fs_030005>-lifnr IS NOT INITIAL.
        lv_bp_number = <fs_030005>-lifnr.
      ELSEIF <fs_030005>-kunnr IS NOT INITIAL.
        lv_bp_number = <fs_030005>-kunnr.
      ENDIF.

      CASE <fs_030005>-doc_type.
        WHEN '1'. "Входящий
          CONCATENATE 'Контрагент:' lv_bp_number <fs_030005>-sender_name INTO lv_line SEPARATED BY space.
          APPEND lv_line TO lt_msg_content.
          APPEND INITIAL LINE TO lt_msg_content.
        WHEN '2'. "Исходящий
          CONCATENATE 'Контрагент:' lv_bp_number <fs_030005>-recip_name INTO lv_line SEPARATED BY space.
          APPEND lv_line TO lt_msg_content.
          APPEND INITIAL LINE TO lt_msg_content.
      ENDCASE.

*     Описание
      IF iv_description IS NOT INITIAL.
        CALL FUNCTION 'IQAPI_WORD_WRAP'
          EXPORTING
            textline            = iv_description
            outputlen           = 245
          TABLES
            out_lines           = lt_wrap
          EXCEPTIONS
            outputlen_too_large = 1
            OTHERS              = 2.

        IF sy-subrc = 0.
          LOOP AT lt_wrap INTO lv_line.
            IF sy-tabix = 1.
              CONCATENATE 'Описание:' lv_line INTO lv_line SEPARATED BY space.
            ENDIF.
            APPEND lv_line TO lt_msg_content.
          ENDLOOP.
        ENDIF.
      ENDIF.

*     Наименование ссылки на карточку
      lv_link_descr = <fs_030005>-doc_id.

*     Определим необходимый GLN для поиска получателей сообщения
      CASE is_msg_routing-runtype.
        WHEN '3'.                           " Документ исходящий
          lv_gln = <fs_030005>-sender_gln.
        WHEN '4'.                           " Документ входящий
          lv_gln = <fs_030005>-recip_gln.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

  ENDCASE.

* Список получателей сообщения
  SELECT * FROM zedi_gln_users INTO TABLE lt_gln_users
    WHERE gln = lv_gln.

  IF sy-subrc = 0.
    LOOP AT lt_gln_users.
      APPEND lt_gln_users-uname TO lt_user_list.
    ENDLOOP.
  ENDIF.

* Если это отмена, то добавим доп. получателей в отправку
  IF is_msg_routing-docstatus = '1'. "Отмена
*   Получим данные маршрута
    SELECT * FROM /dfs/apprdata INTO TABLE lt_apprdata
      WHERE dokar = is_keyfields-dokar
        AND doknr = is_keyfields-doknr
        AND dokvr = is_keyfields-dokvr
        AND doktl = is_keyfields-doktl
        AND aprrole IN ('1','6'). "Инициатор, Обработка
    IF sy-subrc = 0.
      LOOP AT lt_apprdata INTO ls_apprdata.
        APPEND ls_apprdata-proc_user TO lt_user_list.
      ENDLOOP.

      APPEND INITIAL LINE TO lt_msg_content.
      lv_line = '*Внимание! EDI-документ отменен отправителем. Проверьте актуальность зависимых документов! (наличие проведенных документов MIGO, MIRO, статус DFS-договора и т.д.)'.
      APPEND lv_line TO lt_msg_content.
    ENDIF.
  ENDIF.

* Подпись
  APPEND INITIAL LINE TO lt_msg_content.
  CONCATENATE 'Система SAP' sy-sysid INTO lv_line SEPARATED BY space.
  APPEND lv_line TO lt_msg_content.

* Выполняем рассылку на внешнюю почту
  lv_langu = sy-langu.
  sy-langu = 'R'.

  CALL FUNCTION '/DFS/SEND_LETTER'
    EXPORTING
      ix_sap_mail       = lf_sap_mail           " Default ''
      ix_ext_mail       = lf_ext_mail           " Default 'X'
      ix_send_2_zam     = 'X'
      i_msg_subject     = lv_msg_subject        " Default 'Уведомление'
      ix_epress         = 'X'
      i_link_descr      = lv_link_descr         " Default 'Карточка договора'
      i_link_mode       = lv_link_mode
      i_link_runtype    = lv_link_runtype
      is_link_keyfields = is_keyfields
    TABLES
      it_msg_content    = lt_msg_content
      it_user_list      = lt_user_list.

  sy-langu = lv_langu.

ENDFORM.                    "send_notification

*&---------------------------------------------------------------------*
*&      Form  dependent_doc_send_notify
*&---------------------------------------------------------------------*
FORM dependent_doc_send_notify USING iv_objtype          TYPE swo_objtyp
                                     is_keyfields        TYPE /dfs/str_keyfields
                                     is_keyfields_999011 TYPE /dfs/str_keyfields
                                     is_card_data
                                     is_card_data_999011 TYPE zdf_card_999011
                                     is_return           TYPE bapiret2
                                     iv_error.

  DATA: ls_apprdata      TYPE /dfs/apprdata,
        lt_apprdata      TYPE TABLE OF /dfs/apprdata,
        lt_user_list     TYPE bapibname      OCCURS 0 WITH HEADER LINE,
        lv_line          TYPE so_text255,
        lv_doknr(25)     TYPE c,
        lv_bp_number(10) TYPE c,
        lv_date_str(10)  TYPE c,
        lv_langu         TYPE c.

  DATA: ls_items_030003 TYPE zdf_item_030003,
        lt_items_030003 TYPE TABLE OF zdf_item_030003,
        ls_items_030004 TYPE zdf_item_030004,
        lt_items_030004 TYPE TABLE OF zdf_item_030004.

* Define default values and parameters for FM '/DFS/SEND_LETTER'
  DATA: lf_sap_mail(1)     VALUE '',
        lf_ext_mail(1)     VALUE 'X',
        lv_msg_subject(50) VALUE 'Результат обработки эл.документа',
        lv_link_descr(50)  VALUE 'Карточка электронного документа'.

  DATA: lv_link_runtype TYPE /dfs/runtype,
        lv_link_mode    TYPE i,
        lt_msg_content  LIKE solisti1 OCCURS 0 WITH HEADER LINE.

  DATA: lt_wrap TYPE TABLE OF string.

  FIELD-SYMBOLS: <fs_030003> TYPE zdf_card_030003,
                 <fs_030004> TYPE zdf_card_030004,
                 <fs_030005> TYPE zdf_card_030005.

  CASE iv_objtype.
    WHEN z030_if_030003_constants=>main_bo_name. " ---------------------------------------- Электронная накладная ----------------------------------------
    WHEN z030_if_030004_constants=>main_bo_name. " ---------------------------------------- Акт выполненных работ ----------------------------------------
    WHEN z030_if_030005_constants=>main_bo_name. " ---------------------------------------- Электронный документ -----------------------------------------
      ASSIGN is_card_data TO <fs_030005>.
*     Наименование ссылки на карточку
      lv_link_descr = <fs_030005>-doc_id.

*     Дата документа
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          date_internal = <fs_030005>-doc_date
        IMPORTING
          date_external = lv_date_str.

*     Текст:
      CONCATENATE 'Номер документа:' <fs_030005>-doc_id 'от' lv_date_str INTO lv_line SEPARATED BY space.
      CONCATENATE lv_line '.' INTO lv_line.
      APPEND lv_line TO lt_msg_content.
      APPEND INITIAL LINE TO lt_msg_content.

*     Контрагент
      IF <fs_030005>-lifnr IS NOT INITIAL.
        lv_bp_number = <fs_030005>-lifnr.
      ELSEIF <fs_030005>-kunnr IS NOT INITIAL.
        lv_bp_number = <fs_030005>-kunnr.
      ENDIF.

      CASE <fs_030005>-doc_type.
        WHEN '1'. "Входящий
          CONCATENATE 'Контрагент:' lv_bp_number <fs_030005>-sender_name INTO lv_line SEPARATED BY space.
          APPEND lv_line TO lt_msg_content.
        WHEN '2'. "Исходящий
          CONCATENATE 'Контрагент:' lv_bp_number <fs_030005>-recip_name INTO lv_line SEPARATED BY space.
          APPEND lv_line TO lt_msg_content.
      ENDCASE.
      APPEND INITIAL LINE TO lt_msg_content.

*     Номер договора
      lv_doknr = is_keyfields_999011-doknr.
      SHIFT lv_doknr LEFT DELETING LEADING '0'.

      IF iv_error IS NOT INITIAL OR is_return-type = 'E'.
        CONCATENATE lv_msg_subject '. Ошибка!' INTO lv_msg_subject.

        CONCATENATE 'На основании подписанного EDI-документа не удалось перевести карточку договора'
                    lv_doknr 'в статус "Действующий".' INTO lv_line SEPARATED BY space.
        APPEND lv_line TO lt_msg_content.
        lv_line = 'Обработайте документ вручную.'.
        APPEND lv_line TO lt_msg_content.

        IF is_return IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_msg_content.
          lv_line = 'Описание ошибки:'.
          APPEND lv_line TO lt_msg_content.
          MESSAGE ID is_return-id TYPE is_return-type NUMBER is_return-number WITH is_return-message_v1
                                                                                   is_return-message_v2
                                                                                   is_return-message_v3
                                                                                   is_return-message_v4 INTO lv_line.
          APPEND lv_line TO lt_msg_content.
        ENDIF.
      ELSE.
        CONCATENATE 'На основании подписанного EDI-документа договор' lv_doknr 'переведен в статус "Действующий".' INTO lv_line SEPARATED BY space.
        APPEND lv_line TO lt_msg_content.
      ENDIF.
  ENDCASE.

* Список получателей сообщения
  SELECT * FROM /dfs/apprdata INTO TABLE lt_apprdata
    WHERE dokar   = is_keyfields-dokar
      AND doknr   = is_keyfields-doknr
      AND dokvr   = is_keyfields-dokvr
      AND doktl   = is_keyfields-doktl
      AND aprrole = '1'. "Инициатор карточки электронного документа
  IF sy-subrc = 0.
    LOOP AT lt_apprdata INTO ls_apprdata.
      APPEND ls_apprdata-aprname TO lt_user_list.
    ENDLOOP.
  ENDIF.

  SELECT * FROM /dfs/apprdata INTO TABLE lt_apprdata
    WHERE dokar   = is_keyfields_999011-dokar
      AND doknr   = is_keyfields_999011-doknr
      AND dokvr   = is_keyfields_999011-dokvr
      AND doktl   = is_keyfields_999011-doktl
      AND aprrole = '1'. "Инициатор карточки договора
  IF sy-subrc = 0.
    LOOP AT lt_apprdata INTO ls_apprdata.
      APPEND ls_apprdata-aprname TO lt_user_list.
    ENDLOOP.
  ELSE.
    APPEND is_card_data_999011-created_by TO lt_user_list.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM lt_user_list.

* Подпись
  APPEND INITIAL LINE TO lt_msg_content.
  CONCATENATE 'Система SAP' sy-sysid INTO lv_line SEPARATED BY space.
  APPEND lv_line TO lt_msg_content.

* Выполняем рассылку на внешнюю почту
  lv_langu = sy-langu.
  sy-langu = 'R'.

  CALL FUNCTION '/DFS/SEND_LETTER'
    EXPORTING
      ix_sap_mail       = lf_sap_mail           " Default ''
      ix_ext_mail       = lf_ext_mail           " Default 'X'
      ix_send_2_zam     = 'X'
      i_msg_subject     = lv_msg_subject        " Default 'Уведомление'
      ix_epress         = 'X'
      i_link_descr      = lv_link_descr         " Default 'Карточка договора'
      i_link_mode       = lv_link_mode
      i_link_runtype    = lv_link_runtype
      is_link_keyfields = is_keyfields
    TABLES
      it_msg_content    = lt_msg_content
      it_user_list      = lt_user_list.

  sy-langu = lv_langu.

ENDFORM.                    "dependent_doc_send_notify

*&---------------------------------------------------------------------*
*&      Form  parse_emails
*&---------------------------------------------------------------------*
FORM parse_emails CHANGING ct_receivers_list TYPE tt_receivers_list.

  DATA: lw_receivers_list TYPE ts_receivers_list.

  LOOP AT so_addr WHERE sign = 'I' AND option = 'EQ'.
    lw_receivers_list-receiver_addr = so_addr-low.
    APPEND lw_receivers_list TO ct_receivers_list.
  ENDLOOP.

ENDFORM.                    "parse_emails

*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
FORM send_email.

  DATA:
    l_send_request  TYPE REF TO cl_bcs,
    l_document      TYPE REF TO cl_document_bcs,
    l_recipient     TYPE REF TO if_recipient_bcs,
    l_bcs_exception TYPE REF TO cx_bcs,
    l_sent_to_all   TYPE os_boolean,
    l_subject       TYPE so_obj_des.

  DATA:
    lt_receivers_list TYPE tt_receivers_list,
    lw_receivers_list LIKE LINE OF lt_receivers_list,
    lt_email_content  TYPE bcsy_text,
    lw_email_content  TYPE soli.

  CLEAR: lw_email_content,
         lw_receivers_list.

  REFRESH: lt_email_content,
           lt_receivers_list.

  IF so_addr IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM parse_emails CHANGING lt_receivers_list.

  lw_email_content-line = 'Во время обработки EDI-сообщений возникли ошибки!'.
  APPEND lw_email_content TO lt_email_content.
  APPEND INITIAL LINE TO lt_email_content.
  lw_email_content-line = 'Сообщения об ошибках:'.
  APPEND lw_email_content TO lt_email_content.

  LOOP AT gt_errors INTO gs_errors.
    lw_email_content-line = gs_errors-line.
    APPEND lw_email_content TO lt_email_content.
  ENDLOOP.

  TRY.
      "create persistent send request
      l_send_request = cl_bcs=>create_persistent( ).

      CONCATENATE 'Статус обработки EDI-сообщений' sy-sysid INTO l_subject SEPARATED BY space.

      "create document from internal table with text
      l_document = cl_document_bcs=>create_document( i_type    = 'RAW'
                                                     i_subject = l_subject
                                                     i_text    = lt_email_content ).
      "add the document to the send request
      l_send_request->set_document( l_document ).

      "add the recipients to the send request
      LOOP AT lt_receivers_list INTO lw_receivers_list.
        l_recipient = cl_cam_address_bcs=>create_internet_address( i_address_string = lw_receivers_list-receiver_addr ).
        l_send_request->add_recipient( i_recipient = l_recipient ).
      ENDLOOP.

      "send the document
      l_sent_to_all = l_send_request->send( i_with_error_screen = 'X' ).
      COMMIT WORK.

    CATCH cx_bcs INTO l_bcs_exception.
      WRITE: 'Произошла ошибка при отправке сообщения на e-mail!'.
      WRITE: 'Тип ошибки:', l_bcs_exception->error_type.
      EXIT.

  ENDTRY.

ENDFORM.                    "send_email

*&---------------------------------------------------------------------*
*&      Form  dependent_doc_processing
*&---------------------------------------------------------------------*
FORM dependent_doc_processing USING iv_objtype   TYPE swo_objtyp
                                    is_keyfields TYPE /dfs/str_keyfields
                                    is_card_data
                                    iv_status    TYPE /dfs/dokst
                                    is_rcrd_data TYPE /dfs/rcrd_record_data.

  DATA: ls_keyfields   TYPE /dfs/str_keyfields,
        lt_objectlinks TYPE TABLE OF /dfs/drad,
        ls_objectlinks TYPE /dfs/drad,
        lt_drad        TYPE TABLE OF /dfs/drad,
        ls_drad        TYPE /dfs/drad,
        lt_files       TYPE TABLE OF /dfs/rcrd_record_data,
        ls_files       TYPE /dfs/rcrd_record_data,
        lt_cardd       TYPE TABLE OF /dfs/cardd,
        ls_card_data   TYPE zdf_card_999011,
        ls_card_data_x TYPE zdf_card_999011_x,
        ls_rcrd_data   TYPE /dfs/rcrd_record_data,
        lt_kfs         TYPE TABLE OF /dfs/str_keyfields,
        ls_kfs         TYPE /dfs/str_keyfields,
        lv_status      TYPE /dfs/dokst,
        lv_objkey      TYPE swo_typeid,
        lt_return      TYPE TABLE OF bapiret2,
        ls_return      TYPE bapiret2,
        lv_error.

  FIELD-SYMBOLS: <fs_030003> TYPE zdf_card_030003,
                 <fs_030004> TYPE zdf_card_030004,
                 <fs_030005> TYPE zdf_card_030005.

  CASE iv_objtype.
    WHEN z030_if_030003_constants=>main_bo_name.
    WHEN z030_if_030004_constants=>main_bo_name.
    WHEN z030_if_030005_constants=>main_bo_name.
      ASSIGN is_card_data TO <fs_030005>.

*     Если электронный документ переведен в статус "Подтвержден",
*      и существует связанная с ним карточка договора, то
*      нужно перевести карточку договора в статус "Действующий".

      IF iv_status = 'EF' OR "Подтвержден
         iv_status = 'EZ'.   "Отменен

        CALL FUNCTION 'ZDF_API_GET_CARD_INFO_030005'
          EXPORTING
            keyfield             = is_keyfields
          TABLES
            objectlinks          = lt_objectlinks
            documentfiles        = lt_files
          EXCEPTIONS
            card_not_found       = 1
            error_read_card_data = 2
            break_enhancement    = 3
            critical_error       = 4
            application_error    = 5
            OTHERS               = 6.
        IF sy-subrc = 0.
          LOOP AT lt_objectlinks INTO ls_objectlinks WHERE botype = z030_if_999011_constants=>main_bo_name.

            ls_keyfields-dokar = ls_objectlinks-bokey(3).
            ls_keyfields-doknr = ls_objectlinks-bokey+3(25).
            ls_keyfields-dokvr = ls_objectlinks-bokey+28(2).
            ls_keyfields-doktl = ls_objectlinks-bokey+30.

            CALL FUNCTION 'ZDF_API_GET_CARD_INFO_999011'
              EXPORTING
                keyfield             = ls_keyfields
              IMPORTING
                status               = lv_status
                data_card            = ls_card_data
              TABLES
                objectlinks          = lt_drad
              EXCEPTIONS
                card_not_found       = 1
                error_read_card_data = 2
                break_enhancement    = 3
                critical_error       = 4
                application_error    = 5
                OTHERS               = 6.

            IF sy-subrc = 0.
              IF lv_status = 'C4' OR "Согласован
               ( lv_status = 'C1' AND ls_card_data-x_no_appr IS NOT INITIAL ). " Без согласования

                "Создадим ссылки на подписанные файлы документов
                IF iv_status = 'EF'. "Подтвержден
                  CALL FUNCTION '/DFS/GET_CARD_STRKEY'
                    EXPORTING
                      dokar  = ls_keyfields-dokar
                      doknr  = ls_keyfields-doknr
                      dokvr  = ls_keyfields-dokvr
                      doktl  = ls_keyfields-doktl
                    IMPORTING
                      strkey = lv_objkey.

                  LOOP AT lt_files INTO ls_files WHERE doctype = 'SCN'.
                    CALL FUNCTION '/DFS/CP_CREATE_LINK_I'
                      EXPORTING
                        i_objtype        = z030_if_999011_constants=>main_bo_name
                        i_objkey         = lv_objkey
                        i_rel_key        = ls_files-rel_key
                        ix_dbl_check     = 'X'
                        ix_ask_descr     = ' '
                      CHANGING
                        cs_rcrd_data     = ls_rcrd_data
                      EXCEPTIONS
                        operation_failed = 1
                        OTHERS           = 2.
                  ENDLOOP.
                ENDIF.

                "Переводим договор в действующий статус только когда все связанные документы в статусе "Подтвержден"
                DELETE lt_drad WHERE botype NE z030_if_030005_constants=>main_bo_name.

                REFRESH: lt_cardd, lt_kfs.
                LOOP AT lt_drad INTO ls_drad.
                  ls_kfs-dokar = ls_drad-bokey(3).
                  ls_kfs-doknr = ls_drad-bokey+3(25).
                  ls_kfs-dokvr = ls_drad-bokey+28(2).
                  ls_kfs-doktl = ls_drad-bokey+30.
                  APPEND ls_kfs TO lt_kfs.
                ENDLOOP.

                IF lines( lt_kfs ) <> 0.
                  "   Переводим договор в статус "Действующий" только если
                  " есть присоединенный документ в статусе "Подтвержден" и
                  " все остальные документы в статусах "Подтвержден" или "Отменен".

                  SELECT * FROM /dfs/cardd INTO TABLE lt_cardd
                    FOR ALL ENTRIES IN lt_kfs
                    WHERE dokar = lt_kfs-dokar
                      AND doknr = lt_kfs-doknr
                      AND dokvr = lt_kfs-dokvr
                      AND doktl = lt_kfs-doktl
                      AND dokst = 'EF'. "Подтвержден
                  IF sy-subrc <> 0.
                    CONTINUE.
                  ENDIF.

                  SELECT * FROM /dfs/cardd INTO TABLE lt_cardd
                    FOR ALL ENTRIES IN lt_kfs
                    WHERE dokar = lt_kfs-dokar
                      AND doknr = lt_kfs-doknr
                      AND dokvr = lt_kfs-dokvr
                      AND doktl = lt_kfs-doktl
                      AND dokst NOT IN ('EF','EZ'). "Подтвержден, Отменен
                  IF sy-subrc = 0.
                    CONTINUE.
                  ENDIF.
                ENDIF.

                "Дата подписания
                ls_card_data-cn_sign_date = sy-datum.
                ls_card_data_x-cn_sign_date = 'X'.

                CALL FUNCTION 'ZDF_API_CHANGE_CARD_999011'
                  EXPORTING
                    keyfield               = ls_keyfields
                    suppress_enhfm         = ''
                    changeoriginals        = ''
                    changelinks            = ''
                    data_card_x            = ls_card_data_x
                  TABLES
                    return                 = lt_return
                  CHANGING
                    data_card              = ls_card_data
                  EXCEPTIONS
                    card_not_found         = 1
                    table_not_found        = 2
                    data_not_found         = 3
                    keys_not_found         = 4
                    authority_check_error  = 5
                    update_card_error      = 6
                    document_not_exist     = 7
                    select_card_error      = 8
                    dms_document_locked    = 9
                    lock_card_error        = 10
                    change_dms_error       = 11
                    insert_originals_error = 12
                    break_enhancement      = 13
                    critical_error         = 14
                    application_error      = 15
                    alv_data_error         = 16
                    link_error             = 17
                    OTHERS                 = 18.

                "Переводим договор в статус "Действующий" путем нажатия на кнопку "Архивировать"
                CASE lv_status.
                  WHEN 'C1'.
                    CALL FUNCTION '/DFS/MAINTAIN_CARD'
                      EXPORTING
                        mode              = 2
                        s_keyfield        = ls_keyfields
                        runtype           = 6
                        okcode            = 'BT_CUST001' "BT_ACTIVE_PODP
                        ix_catch_messages = 'X'
                      IMPORTING
                        es_return         = ls_return
                      EXCEPTIONS
                        card_not_found    = 1
                        OTHERS            = 2.
                    IF sy-subrc <> 0.
                      lv_error = abap_true.
                    ENDIF.
                  WHEN 'C4'.
                    CALL FUNCTION '/DFS/MAINTAIN_CARD'
                      EXPORTING
                        mode              = 2
                        s_keyfield        = ls_keyfields
                        runtype           = 3
                        aprnr             = 1            "Инициатор
                        okcode            = 'BT_CUST001' "BT_ACTIVE_PODP
                        ix_catch_messages = 'X'
                      IMPORTING
                        es_return         = ls_return
                      EXCEPTIONS
                        card_not_found    = 1
                        OTHERS            = 2.
                    IF sy-subrc <> 0.
                      lv_error = abap_true.
                    ENDIF.
                ENDCASE.

                " Отправим уведомление Инициатору
                PERFORM dependent_doc_send_notify USING iv_objtype
                                                        is_keyfields
                                                        ls_keyfields
                                                        is_card_data
                                                        ls_card_data
                                                        ls_return
                                                        lv_error.
              ENDIF.
            ENDIF.

          ENDLOOP.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM.
