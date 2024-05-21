*&---------------------------------------------------------------------*
*&  Include           ZVLC_BAL_LOG
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  show_log
*&---------------------------------------------------------------------*
FORM show_log TABLES ct_msg TYPE bal_t_msg.

* Таблица для вывода log-сообщений
  TYPES: BEGIN OF type_log,
    excpt TYPE n,
    text(400) TYPE c,
  END OF type_log.

  DATA: lt_log TYPE STANDARD TABLE OF type_log,
        ls_log LIKE LINE OF lt_log.

  DATA: lref_log     TYPE REF TO cl_salv_table,
        lr_functions TYPE REF TO cl_salv_functions_list,
        lr_columns   TYPE REF TO cl_salv_columns_table,
        lr_column    TYPE REF TO cl_salv_column_table.

  DATA: ls_msg TYPE bal_s_msg.

* Создадим лог-таблицу
  LOOP AT ct_msg INTO ls_msg.
    CLEAR ls_log.

    MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno INTO ls_log-text
      WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.

    CASE ls_msg-msgty.
      WHEN 'A' OR 'X' OR 'E'.
        ls_log-excpt = 1.
      WHEN 'W'.
        ls_log-excpt = 2.
      WHEN 'I' OR 'S'.
        ls_log-excpt = 3.
    ENDCASE.

    APPEND ls_log TO lt_log.
  ENDLOOP.

* Покажем лог-таблицу
  CHECK lt_log IS NOT INITIAL.
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lref_log
        CHANGING
          t_table      = lt_log.
    CATCH cx_salv_msg.
  ENDTRY.

  lr_columns = lref_log->get_columns( ).
  lr_columns->set_exception_column( 'EXCPT' ).

  lr_column ?= lr_columns->get_column( columnname = 'TEXT' ).
  lr_column->set_output_length( value = 150 ).
  lr_column->set_long_text( value = 'Текст сообщения' ).

  lr_functions = lref_log->get_functions( ).
  lr_functions->set_all( value = if_salv_c_bool_sap=>true ).

  lref_log->display( ).

ENDFORM.                    "show_log

*&---------------------------------------------------------------------*
*&      Form  write_log
*&---------------------------------------------------------------------*
FORM write_log TABLES ct_msg TYPE bal_t_msg
                USING iv_object TYPE balobj_d
                      iv_subobject TYPE balsubobj.

  DATA: lv_log_handle TYPE balloghndl,
        lt_log_handle TYPE bal_t_logh.

  DATA: ls_log TYPE bal_s_log,
        ls_msg TYPE bal_s_msg.

  ls_log-object     = iv_object.
  ls_log-subobject  = iv_subobject.
  ls_log-aluser     = sy-uname.
  ls_log-alprog     = sy-repid.
  ls_log-altcode    = sy-tcode.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = ls_log
    IMPORTING
      e_log_handle            = lv_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Ошибка записи сообщений в журнал!' TYPE 'S'.
  ENDIF.

  LOOP AT ct_msg INTO ls_msg.
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = lv_log_handle
        i_s_msg          = ls_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      MESSAGE 'Ошибка записи сообщений в журнал!' TYPE 'S'.
    ENDIF.
  ENDLOOP.

  APPEND lv_log_handle TO lt_log_handle.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_save_all       = 'X'
      i_t_log_handle   = lt_log_handle
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.

  CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
    EXPORTING
      i_log_handle  = lv_log_handle
    EXCEPTIONS
      log_not_found = 1
      OTHERS        = 2.

ENDFORM.                    "write_log

*&---------------------------------------------------------------------*
*&      Form  add_msg
*&---------------------------------------------------------------------*
FORM add_msg USING iv_msgty
                   iv_msgid
                   iv_msgno
                   iv_msgv1
                   iv_msgv2
                   iv_msgv3
                   iv_msgv4
          CHANGING it_msg TYPE bal_t_msg.

  DATA: l_s_msg TYPE bal_s_msg.

  l_s_msg-msgty = iv_msgty.
  l_s_msg-msgid = iv_msgid.
  l_s_msg-msgno = iv_msgno.

  IF iv_msgv1 IS NOT INITIAL.
    WRITE iv_msgv1 TO l_s_msg-msgv1 LEFT-JUSTIFIED NO-ZERO. CONDENSE l_s_msg-msgv1.
  ENDIF.
  IF iv_msgv2 IS NOT INITIAL.
    WRITE iv_msgv2 TO l_s_msg-msgv2 LEFT-JUSTIFIED NO-ZERO. CONDENSE l_s_msg-msgv2.
  ENDIF.
  IF iv_msgv3 IS NOT INITIAL.
    WRITE iv_msgv3 TO l_s_msg-msgv3 LEFT-JUSTIFIED NO-ZERO. CONDENSE l_s_msg-msgv3.
  ENDIF.
  IF iv_msgv4 IS NOT INITIAL.
    WRITE iv_msgv4 TO l_s_msg-msgv4 LEFT-JUSTIFIED NO-ZERO. CONDENSE l_s_msg-msgv4.
  ENDIF.

  APPEND l_s_msg TO it_msg.

ENDFORM.                    "add_msg

*&---------------------------------------------------------------------*
*&      Form  add_msg_without_converting
*&---------------------------------------------------------------------*
FORM add_msg_without_converting USING iv_msgty
                                      iv_msgid
                                      iv_msgno
                                      iv_msgv1
                                      iv_msgv2
                                      iv_msgv3
                                      iv_msgv4
                             CHANGING it_msg TYPE bal_t_msg.

  DATA: l_s_msg TYPE bal_s_msg.

  l_s_msg-msgty = iv_msgty.
  l_s_msg-msgid = iv_msgid.
  l_s_msg-msgno = iv_msgno.
  l_s_msg-msgv1 = iv_msgv1.
  l_s_msg-msgv2 = iv_msgv2.
  l_s_msg-msgv3 = iv_msgv3.
  l_s_msg-msgv4 = iv_msgv4.

  APPEND l_s_msg TO it_msg.

ENDFORM.                    "add_msg_without_converting

*&---------------------------------------------------------------------*
*&      Form  wrap_msg
*&---------------------------------------------------------------------*
FORM wrap_msg USING iv_msg
           CHANGING cv_msg1
                    cv_msg2
                    cv_msg3
                    cv_msg4.

  DATA: lv_line(50)  TYPE c,
        lv_text(200) TYPE c,
        lt_wrap      TYPE TABLE OF string.

  lv_text = iv_msg.

  CLEAR: cv_msg1,
         cv_msg2,
         cv_msg3,
         cv_msg4.

  CALL FUNCTION 'IQAPI_WORD_WRAP'
    EXPORTING
      textline            = lv_text
      outputlen           = 50
    TABLES
      out_lines           = lt_wrap
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.

  IF sy-subrc = 0.
    LOOP AT lt_wrap INTO lv_line.
      CASE sy-tabix.
        WHEN 1.
          cv_msg1 = lv_line.
        WHEN 2.
          cv_msg2 = lv_line.
        WHEN 3.
          cv_msg3 = lv_line.
        WHEN 4.
          cv_msg4 = lv_line.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "wrap_msg

*&---------------------------------------------------------------------*
*&      Form  delete_msg
*&---------------------------------------------------------------------*
FORM delete_msg USING iv_msg_id
                      iv_msg_number
             CHANGING it_msg TYPE bal_t_msg.

  DELETE it_msg
    WHERE msgid = iv_msg_id
      AND msgno = iv_msg_number.

ENDFORM.                    "delete_msg

*&---------------------------------------------------------------------*
*&      Form  show_msg_table
*&---------------------------------------------------------------------*
FORM show_msg_table TABLES it_msg TYPE bal_t_msg
                  CHANGING cv_log_handle TYPE balloghndl.

  DATA: l_s_log TYPE bal_s_log,
        l_s_msg TYPE bal_s_msg.

  DATA: l_s_display_profile TYPE bal_s_prof,
        l_s_log_filter      TYPE bal_s_lfil.

  DATA: lrt_log_handle TYPE bal_r_logh WITH HEADER LINE.

  IF LINES( it_msg ) = 0.
    EXIT.
  ENDIF.

  IF cv_log_handle IS INITIAL.
    l_s_log-extnumber  = 'Application log'.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = l_s_log
      IMPORTING
        e_log_handle = cv_log_handle
      EXCEPTIONS
        OTHERS       = 1.
  ELSE.
    CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
      EXPORTING
        i_log_handle  = cv_log_handle
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
  ENDIF.

  IF sy-subrc = 0.
    LOOP AT it_msg INTO l_s_msg.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = cv_log_handle
          i_s_msg      = l_s_msg
        EXCEPTIONS
          OTHERS       = 1.
    ENDLOOP.

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = l_s_display_profile
      EXCEPTIONS
        OTHERS              = 1.

    IF sy-subrc = 0.
      l_s_display_profile-use_grid = 'X'.
      l_s_display_profile-disvariant-report = sy-repid.
      l_s_display_profile-disvariant-handle = 'LOG'.

      lrt_log_handle-sign = 'I'.
      lrt_log_handle-option = 'EQ'.
      lrt_log_handle-low = cv_log_handle.
      APPEND lrt_log_handle TO l_s_log_filter-log_handle.

      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_s_display_profile = l_s_display_profile
          i_s_log_filter      = l_s_log_filter
        EXCEPTIONS
          OTHERS              = 1.
    ENDIF.
  ENDIF.

ENDFORM.                    "show_msg_table
