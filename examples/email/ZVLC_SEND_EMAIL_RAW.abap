PERFORM create_attachment TABLES lt_fields
                                 lt_cells
                                 gt_lv_data
                           USING 'DATA_TABLE'
                                 c_kappl_lv
                                 c_kschl_lv
                        CHANGING lt_attachmment_inv.

* Приложение
IF p_app EQ abap_true.
  PERFORM app_data_get   USING <lw_pernr>.
  PERFORM app_data_print USING <lw_pernr>
                      CHANGING lt_attachmment_app.
ENDIF.

PERFORM email_inventory USING lt_attachmment_inv
                              lt_attachmment_app
                              l_mol_name
                              <lw_pernr>.



FORM create_attachment TABLES it_fields
                              it_cells
                              it_table_data
                        USING uv_table_name
                              uv_kappl TYPE kappl
                              uv_kschl TYPE kschl
                     CHANGING ct_attachmment TYPE solix_tab.

  DATA:
    retcode           TYPE soi_ret_string,
    container_control TYPE REF TO i_oi_container_control,
    error             TYPE REF TO i_oi_error.

  DATA:
    lr_send_request  TYPE REF TO cl_bcs,
    lr_document      TYPE REF TO cl_document_bcs,
    lr_recipient     TYPE REF TO if_recipient_bcs,
    lr_bcs_exception TYPE REF TO cx_bcs,
    lv_sent          TYPE os_boolean,
    lv_smtp_addr     TYPE ad_smtpadr,
    lv_attach_size   TYPE i,
    ls_messages      LIKE LINE OF gt_messages,
    lt_email_content TYPE bcsy_text.

  IF g_template IS INITIAL.
    CALL METHOD c_oi_container_control_creator=>get_container_control
      IMPORTING
        control = container_control
        retcode = retcode
        error   = error.
    CALL METHOD c_oi_errors=>raise_message
      EXPORTING
        type = 'E'.

    g_container_control_ole ?= container_control.

    CREATE OBJECT g_container
      EXPORTING
        container_name = 'CONTAINER'.

    CALL METHOD g_container_control_ole->init_control
      EXPORTING
        r3_application_name = 'Print'
        parent              = g_container
      IMPORTING
        retcode             = retcode.
    CALL METHOD c_oi_errors=>raise_message
      EXPORTING
        type = 'E'.

    IF g_bds_instance IS INITIAL.
      CREATE OBJECT g_bds_instance.
    ENDIF.

    CREATE OBJECT g_template
      EXPORTING
        control      = g_container_control_ole
        bds_instance = g_bds_instance
        doc_type     = 'Excel.Sheet.8'
        doc_format   = soi_docformat_compound.
  ENDIF.

  CALL METHOD g_template->open_document
    EXPORTING
      open_inplace = space
      kappl        = uv_kappl
      kschl        = uv_kschl.

  IF NOT it_cells[] IS INITIAL.
    CALL METHOD g_template->xls_cells_transfer
      EXPORTING
        cells   = it_cells[]
      IMPORTING
        error   = error
        retcode = retcode.
  ENDIF.

  IF NOT it_fields[] IS INITIAL.
    CALL METHOD g_template->xls_fields_transfer
      EXPORTING
        fields  = it_fields[]
      IMPORTING
        error   = error
        retcode = retcode.
  ENDIF.

  IF NOT it_table_data[] IS INITIAL.
    CALL METHOD g_template->insert_range
      EXPORTING
        name    = uv_table_name
        data    = it_table_data[]
      IMPORTING
        error   = error
        retcode = retcode.
  ENDIF.

  CALL METHOD g_template->get_proxy
    RECEIVING
      retobj = g_proxy.

  CALL METHOD g_proxy->save_document_to_table
    IMPORTING
      error          = error
    CHANGING
      document_table = ct_attachmment
      document_size  = lv_attach_size.

  CALL METHOD c_oi_errors=>raise_message
    EXPORTING
      type = 'E'.

  CALL METHOD g_template->close_document
    EXPORTING
      do_save = space
    IMPORTING
      retcode = retcode.

  CALL METHOD c_oi_errors=>raise_message
    EXPORTING
      type = 'E'.

ENDFORM.




FORM email_inventory USING ut_attachmment_inv TYPE solix_tab
                           ut_attachmment_app TYPE solix_tab
                           uv_fname TYPE so_obj_des
                           uv_pernr TYPE pernr_d.

  DATA:
    retcode           TYPE soi_ret_string,
    container_control TYPE REF TO i_oi_container_control,
    error             TYPE REF TO i_oi_error.

  DATA:
    lr_send_request  TYPE REF TO cl_bcs,
    lr_document      TYPE REF TO cl_document_bcs,
    lr_recipient     TYPE REF TO if_recipient_bcs,
    lr_bcs_exception TYPE REF TO cx_bcs,
    lv_sent          TYPE os_boolean,
    lv_smtp_addr     TYPE ad_smtpadr,
    lv_attach_size   TYPE i,
    ls_messages      LIKE LINE OF gt_messages,
    lt_attachmment   TYPE solix_tab,
    lt_email_content TYPE bcsy_text.

  TRY.
      lr_send_request = cl_bcs=>create_persistent( ).

      PERFORM create_email_content CHANGING lt_email_content.

      lr_document = cl_document_bcs=>create_document(
            i_type    = 'RAW'
            i_subject = 'Инвентаризационная ведомость'(024)
            i_text    = lt_email_content ).

      IF ut_attachmment_inv IS NOT INITIAL.
        CALL METHOD lr_document->add_attachment
          EXPORTING
            i_attachment_type    = 'xls'
            i_att_content_hex    = ut_attachmment_inv
            i_attachment_subject = uv_fname.

        lr_send_request->set_document( lr_document ).
      ENDIF.

      IF ut_attachmment_app IS NOT INITIAL.
        CALL METHOD lr_document->add_attachment
          EXPORTING
            i_attachment_type    = 'xls'
            i_att_content_hex    = ut_attachmment_app
            i_attachment_subject = 'Приложение'.

        lr_send_request->set_document( lr_document ).
      ENDIF.

      PERFORM get_smtp_addr USING uv_pernr CHANGING lv_smtp_addr.

      lr_recipient = cl_cam_address_bcs=>create_internet_address(
        i_address_string = lv_smtp_addr ).

      lr_send_request->add_recipient( i_recipient = lr_recipient ).

      CLEAR lv_sent.
      lv_sent = lr_send_request->send( ).
      IF lv_sent = abap_true.
        ls_messages-msgid = 'BCS_MEDIUM'.
        ls_messages-msgty = 'S'.
        ls_messages-msgno = '004'.
        ls_messages-msgv1 = lv_smtp_addr.
        APPEND ls_messages TO gt_messages.
      ENDIF.

      COMMIT WORK.

    CATCH cx_send_req_bcs.
      ls_messages-msgid = 'BCS_MEDIUM'.
      ls_messages-msgty = 'E'.
      ls_messages-msgno = '005'.
      ls_messages-msgv1 = lv_smtp_addr.
      APPEND ls_messages TO gt_messages.
      RETURN.
    CATCH cx_document_bcs.
      ls_messages-msgid = 'BCS_MEDIUM'.
      ls_messages-msgty = 'E'.
      ls_messages-msgno = '005'.
      ls_messages-msgv1 = lv_smtp_addr.
      APPEND ls_messages TO gt_messages.
      RETURN.
    CATCH cx_address_bcs.
      ls_messages-msgid = 'BCS_MEDIUM'.
      ls_messages-msgty = 'E'.
      ls_messages-msgno = '005'.
      ls_messages-msgv1 = lv_smtp_addr.
      APPEND ls_messages TO gt_messages.
      RETURN.

  ENDTRY.

ENDFORM.                    " EMAIL_INVENTORY