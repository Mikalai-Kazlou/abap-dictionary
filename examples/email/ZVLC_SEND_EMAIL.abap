*&---------------------------------------------------------------------*
*&  Include           Z030_SEND_MAIL
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       Send email
*----------------------------------------------------------------------*
*  -->  UV_EMAIL_SUBJECT - Email subject
*  -->  UT_EMAIL_CONTENT - Email content
*  -->  UT_RECEIVER_ADDR - Email receiver
*----------------------------------------------------------------------*
FORM send_email USING uv_email_subject TYPE so_obj_des
                      ut_email_content TYPE bcsy_text
                      ut_receiver_addr TYPE ad_smtpadr.

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
        i_type    = 'RAW'
        i_subject = uv_email_subject
        i_text    = ut_email_content ).

* add the document to the send request
      lv_send_request->set_document( lv_document ).

* add the recipients to the send request
      lv_recipient = cl_cam_address_bcs=>create_internet_address( i_address_string = ut_receiver_addr ).
      lv_send_request->add_recipient( i_recipient = lv_recipient ).

* -- send the document --
      lv_send_request->set_send_immediately( abap_false ).
      lv_send_request->send( ).

      COMMIT WORK.

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
  DATA:
    ls_bapiaddr3 TYPE bapiaddr3,
    lt_bapiret2  TYPE TABLE OF bapiret2.

  CLEAR: ls_bapiaddr3, lt_bapiret2.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = uv_uname
    IMPORTING
      address  = ls_bapiaddr3
    TABLES
      return   = lt_bapiret2.

  cv_email = ls_bapiaddr3-e_mail.

ENDFORM.                    " GET_EMAIL_ADDRESS

*&---------------------------------------------------------------------*
*&      Form  check_user_active
*&---------------------------------------------------------------------*
FORM check_user_active USING uv_uname  TYPE uname
                    CHANGING cv_active TYPE abap_bool.

  DATA: lv_user TYPE xubname.

  CLEAR: cv_active.

  SELECT SINGLE bname FROM usr02 INTO lv_user
    WHERE bname = uv_uname
      AND ( uflag = 0 OR uflag = 64 OR uflag = 128 ). " 0 - not locked, 128 - locked because of incorrect logon

  IF sy-subrc = 0.
    cv_active = abap_true.
  ENDIF.

ENDFORM.                    " check_user_active
