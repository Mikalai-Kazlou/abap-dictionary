DATA: lt_group_data        TYPE tt_data,
      lt_data_email        TYPE tt_data_email,
      lt_email_content     TYPE bcsy_text,
      lv_email_subject     TYPE so_obj_des VALUE 'Уведомление о приближении оплаты в счет поставщика',

PERFORM create_email_content USING lt_group_data
	                  CHANGING lt_email_content
	                           lt_data_email.

PERFORM send_email USING lv_email_subject
                         lt_email_content
                         group_ref->address
		CHANGING lv_result.

*&---------------------------------------------------------------------*
*&      Form  CREATE_EMAIL_CONTENT
*&---------------------------------------------------------------------*
FORM create_email_content USING ut_data TYPE tt_data
                       CHANGING ct_email_content TYPE bcsy_text
                                ct_data_email TYPE tt_data_email.

  DATA:
    lt_column_name TYPE tt_column_name,
    ls_column_name TYPE ts_column_name,
    lt_email_txt   TYPE tt_email_txt,
    ls_email_txt   TYPE ts_email_txt,
    lt_data_email  TYPE tt_data_email,
    ls_data_email  TYPE ts_data_email,
    ls_data        TYPE ts_data,
    lt_header      TYPE STANDARD TABLE OF w3head,
    lt_fields      TYPE STANDARD TABLE OF w3fields,
    lt_html        TYPE STANDARD TABLE OF w3html,
    ls_html        TYPE w3html,
    lv_description TYPE string.

* Шапка
  ls_column_name-text = 'БЕ'.             APPEND ls_column_name TO lt_column_name.
  ls_column_name-text = 'Документ'.       APPEND ls_column_name TO lt_column_name.
  ls_column_name-text = 'Год'.            APPEND ls_column_name TO lt_column_name.
  ls_column_name-text = 'К оплате'.       APPEND ls_column_name TO lt_column_name.
  ls_column_name-text = 'Сумма'.          APPEND ls_column_name TO lt_column_name.
  ls_column_name-text = 'Валюта'.         APPEND ls_column_name TO lt_column_name.
  ls_column_name-text = 'Договор'.        APPEND ls_column_name TO lt_column_name.
  ls_column_name-text = 'Кредитор'.       APPEND ls_column_name TO lt_column_name.
  ls_column_name-text = 'Наименование'.   APPEND ls_column_name TO lt_column_name.

  LOOP AT lt_column_name INTO ls_column_name.
    CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'
      EXPORTING
        field_nr = sy-tabix
        text     = ls_column_name-text
        fgcolor  = 'black'
        size     = '2'
        font     = 'Vernada'
      TABLES
        header   = lt_header.

    CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'
      EXPORTING
        field_nr = sy-tabix
        fgcolor  = 'black'
        size     = '2'
        font     = 'Vernada'
      TABLES
        fields   = lt_fields.
  ENDLOOP.

* Текст
  ls_html-line = 'Добрый день!'.
  APPEND ls_html-line TO lt_email_txt.
  ls_html-line = '</p>'.
  APPEND ls_html-line TO lt_email_txt.
  IF p_exp IS INITIAL. "09.09.2020   Klimakov   ITCR-34618 Уведомления о просроченных оплатах в счёт поставщиков
    ls_html-line = 'Приближается срок оплаты по документам MIRO, указанным в таблице.'.
    APPEND ls_html-line TO lt_email_txt.
    ls_html-line = '</br>'.
    APPEND ls_html-line TO lt_email_txt.
    ls_html-line = 'Во избежание нарушения срока необходимо снять блокировку платежа.'.
    APPEND ls_html-line TO lt_email_txt.
    ls_html-line = '</p>'.
    APPEND ls_html-line TO lt_email_txt.
  ELSE. "09.09.2020   Klimakov   ITCR-34618 Уведомления о просроченных оплатах в счёт поставщиков
    ls_html-line = 'Просрочен срок оплаты по документам MIRO, указанным в таблице.'.
    APPEND ls_html-line TO lt_email_txt.
    ls_html-line = '</br>'.
    APPEND ls_html-line TO lt_email_txt.
    ls_html-line = 'Во избежание нарушения необходимо снять блокировку платежа.'.
    APPEND ls_html-line TO lt_email_txt.
    ls_html-line = '</p>'.
    APPEND ls_html-line TO lt_email_txt.
  ENDIF. "09.09.2020   Klimakov   ITCR-34618 Уведомления о просроченных оплатах в счёт поставщиков

* Таблица
  LOOP AT ut_data INTO ls_data.
    CLEAR: ls_data_email.

    ls_data_email-bukrs = ls_data-bukrs.
    ls_data_email-belnr = ls_data-belnr.
    ls_data_email-gjahr = ls_data-gjahr.
    ls_data_email-waers = ls_data-waers.

    WRITE ls_data-fdtag TO ls_data_email-fdtag DD/MM/YYYY.
    WRITE ls_data-wrbtr TO ls_data_email-wrbtr RIGHT-JUSTIFIED CURRENCY ls_data-waers.

    ls_data_email-vertn = ls_data-vertn.
    SHIFT ls_data_email-vertn LEFT DELETING LEADING '0'.

    ls_data_email-lifnr = ls_data-lifnr.
    SHIFT ls_data_email-lifnr LEFT DELETING LEADING '0'.

    CALL FUNCTION 'Z030_GET_LIFNR_KUNNR_DESCR'
      EXPORTING
        i_lifnr         = ls_data-lifnr
      IMPORTING
        e_description   = lv_description
      EXCEPTIONS
        wrong_parameter = 1
        data_not_found  = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      ls_data_email-lifnm = lv_description.
    ENDIF.

    APPEND ls_data_email TO lt_data_email.
  ENDLOOP.

  CALL FUNCTION 'WWW_ITAB_TO_HTML'
    TABLES
      html       = lt_html
      fields     = lt_fields
      row_header = lt_header
      itable     = lt_data_email.

  LOOP AT lt_html INTO ls_html.
    APPEND ls_html-line TO lt_email_txt.
  ENDLOOP.

* Подпись
  ls_html-line = '</p>'.
  APPEND ls_html-line TO lt_email_txt.
  CONCATENATE 'Система SAP' sy-sysid INTO ls_html-line SEPARATED BY space.
  APPEND ls_html-line TO lt_email_txt.

  ct_email_content[] = lt_email_txt[].
  ct_data_email[]    = lt_data_email[].

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       Send email
*----------------------------------------------------------------------*
*  -->  UV_EMAIL_SUBJECT - Email subject
*  -->  UT_EMAIL_CONTENT - Email content
*  -->  UV_RECEIVER_ADDR - Email receiver
*----------------------------------------------------------------------*
FORM send_email USING uv_email_subject TYPE so_obj_des
                      ut_email_content TYPE bcsy_text
                      uv_receiver_addr TYPE ad_smtpadr
             CHANGING cv_result        TYPE n.

  DATA:
    lv_send_request     TYPE REF TO cl_bcs,
    lv_document         TYPE REF TO cl_document_bcs,
    lv_recipient        TYPE REF TO if_recipient_bcs,

    lo_exc_doc_bcs      TYPE REF TO cx_document_bcs,
    lo_exc_send_req_bcs TYPE REF TO cx_send_req_bcs,
    lo_exc_address_bcs  TYPE REF TO cx_address_bcs,

    lv_error_text       TYPE string.

  TRY.
* -- create persistent send request --
      lv_send_request = cl_bcs=>create_persistent( ).

* create document from internal table with text
      lv_document = cl_document_bcs=>create_document(
        i_type    = 'HTM'
        i_subject = uv_email_subject
        i_text    = ut_email_content ).

* add the document to the send request
      lv_send_request->set_document( lv_document ).

* add the recipients to the send request
      lv_recipient = cl_cam_address_bcs=>create_internet_address( i_address_string = uv_receiver_addr ).
      lv_send_request->add_recipient( i_recipient = lv_recipient ).

* -- send the document --
*     lv_send_request->set_send_immediately( abap_true ).
      lv_send_request->send( ).

      COMMIT WORK.
      cv_result = 2.

* -- exception handling --
    CATCH cx_document_bcs INTO lo_exc_doc_bcs.
      lv_error_text = lo_exc_doc_bcs->get_text( ).
      WRITE lv_error_text.
    CATCH cx_send_req_bcs INTO lo_exc_send_req_bcs.
      lv_error_text = lo_exc_send_req_bcs->get_text( ).
      WRITE lv_error_text.
    CATCH cx_address_bcs INTO lo_exc_address_bcs.
      lv_error_text = lo_exc_address_bcs->get_text( ).
      WRITE lv_error_text.
  ENDTRY.

ENDFORM.                    " SEND_EMAIL

*&---------------------------------------------------------------------*
*&      Form  GET_EMAIL_ADDRESS
*&---------------------------------------------------------------------*
*       Get user's email
*----------------------------------------------------------------------*
*      --> UV_NAME  - Login (from user master data)
*      <-- CV_EMAIL - User's email
*----------------------------------------------------------------------*
FORM get_email_address USING uv_uname TYPE uname
                    CHANGING cv_email TYPE ad_smtpadr.

  DATA: ls_bapiaddr3 TYPE bapiaddr3,
        lt_bapiret2  TYPE TABLE OF bapiret2.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = uv_uname
    IMPORTING
      address  = ls_bapiaddr3
    TABLES
      return   = lt_bapiret2.

  cv_email = ls_bapiaddr3-e_mail.

ENDFORM.                    " GET_EMAIL_ADDRESS