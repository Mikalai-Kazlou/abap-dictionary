*&---------------------------------------------------------------------*
*&  Include           ZEDI_DELIVERY_NOTE_DFS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  create_card
*&---------------------------------------------------------------------*
FORM create_card TABLES ct_items       STRUCTURE zdf_item_030003
                        ct_objectlinks STRUCTURE /dfs/drad
                        ct_msg    TYPE bal_t_msg
                  USING is_030003 TYPE zdf_card_030003
                        if_open_card.

  DATA: ls_keyfields TYPE /dfs/str_keyfields,
        lv_status    TYPE /dfs/dokst.

  DATA: lt_bapiret2 TYPE bapiret2 OCCURS 0 WITH HEADER LINE.

* Создадим карточку
  CALL FUNCTION 'ZDF_API_CREATE_CARD_030003'
    EXPORTING
      mode                   = 'CREATE'
      commit                 = 'X'
    TABLES
      objectlinks            = ct_objectlinks
      return                 = lt_bapiret2
      items                  = ct_items
    CHANGING
      data_card              = is_030003
      keyfield               = ls_keyfields
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

  IF sy-subrc NE 0.
*   Добавим сообщения
    PERFORM add_msg USING 'E' 'Z030_EDI' '005' '' '' '' ''
                 CHANGING ct_msg[].

    LOOP AT lt_bapiret2.
      IF lt_bapiret2-type = 'E'.
        PERFORM add_msg USING lt_bapiret2-type
                              lt_bapiret2-id
                              lt_bapiret2-number
                              lt_bapiret2-message_v1
                              lt_bapiret2-message_v2
                              lt_bapiret2-message_v3
                              lt_bapiret2-message_v4
                     CHANGING ct_msg[].
      ENDIF.
    ENDLOOP.

    IF if_open_card IS NOT INITIAL.
      MESSAGE e005(z030_edi).
    ENDIF.
  ELSE.
    IF if_open_card IS NOT INITIAL.
*     Откроем карточку для дальнейшего редактирования и отправки
      CALL FUNCTION '/DFS/MAINTAIN_CARD'
        EXPORTING
          card           = '030003'
          mode           = 2
          s_keyfield     = ls_keyfields
          runtype        = 3
        EXCEPTIONS
          card_not_found = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.
        MESSAGE e004(z030_edi).
      ENDIF.
    ELSE.
      CALL FUNCTION '/DFS/CHANGE_DOC_STATUS3'
        EXPORTING
          is_keyfields = ls_keyfields
          i_status_new = 'E9'. " Готов к отправке
    ENDIF.
  ENDIF.

ENDFORM.                    "create_card

*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
FORM check_dfs_data TABLES ct_items  STRUCTURE zdf_item_030003
                           ct_msg    TYPE bal_t_msg
                     USING is_030003 TYPE zdf_card_030003.

  DATA: lt_fields TYPE TABLE OF /dfs/field_attr,
        ls_fields TYPE /dfs/field_attr,
        lv_attr_name(30) TYPE c.

  FIELD-SYMBOLS: <lv_cmp> TYPE ANY.

* Проверим заполнение недоступных обязательных полей
  PERFORM get_required_fields_table TABLES lt_fields.

  LOOP AT lt_fields INTO ls_fields.
    ASSIGN COMPONENT ls_fields-fieldname OF STRUCTURE is_030003 TO <lv_cmp>.
    IF <lv_cmp> IS ASSIGNED.
      IF <lv_cmp> IS INITIAL.

*       Получим наименование проверяемого поля
        PERFORM get_attr_name USING ls_fields
                           CHANGING lv_attr_name.

*       Добавим сообщение
        PERFORM add_msg USING 'E' 'Z030_EDI' '018' lv_attr_name '' '' ''
                     CHANGING ct_msg[].
      ENDIF.
    ENDIF.
  ENDLOOP.

* Проверим наличие товарных позиций
  IF LINES( ct_items ) = 0.
    PERFORM add_msg USING 'E' 'Z030_EDI' '017' '' '' '' ''
                 CHANGING ct_msg[].
  ENDIF.

ENDFORM.                    "check_data

*&---------------------------------------------------------------------*
*&      Form  get_required_fields_table
*&---------------------------------------------------------------------*
FORM get_required_fields_table TABLES ct_fields STRUCTURE /dfs/field_attr.

  DATA: lt_fields_rt1 TYPE TABLE OF /dfs/field_attr,
        lt_fields_rt3 TYPE TABLE OF /dfs/field_attr.

  SELECT * FROM /dfs/field_attr INTO TABLE lt_fields_rt1
    WHERE cardnum   = '030003'    " Электронная накладная
      AND runtype   = '1'         " Создание
      AND dokst     = 'E1'        " Проект
      AND attribute = 'R'.        " Рекомендованный ввод

  IF lt_fields_rt1 IS NOT INITIAL.
    SELECT * FROM /dfs/field_attr INTO TABLE lt_fields_rt3
      FOR ALL ENTRIES IN lt_fields_rt1
      WHERE cardnum   = lt_fields_rt1-cardnum    " Электронная накладная
        AND runtype   = '3'                      " Накладная исходящая
        AND dokst     = lt_fields_rt1-dokst      " Проект
        AND fieldname = lt_fields_rt1-fieldname  " Наименование поля
        AND attribute = '*'.                     " Просмотреть поле
  ENDIF.

  ct_fields[] = lt_fields_rt3.

ENDFORM.                    "get_required_fields_table

*&---------------------------------------------------------------------*
*&      Form  get_attr_name
*&---------------------------------------------------------------------*
FORM get_attr_name USING is_field TYPE /dfs/field_attr
                CHANGING cv_attr_name.

  DATA: lv_data_element TYPE /dfs/rollname,
        lv_scrtext_m    TYPE scrtext_m.

  cv_attr_name = is_field-fieldname.

* Получим data elenent проверяемого поля
  SELECT SINGLE data_elem FROM /dfs/appl INTO lv_data_element
    WHERE appl      = is_field-appl
      AND fieldname = is_field-fieldname.

  IF sy-subrc = 0.
*   Получим среднюю метку поля
    SELECT SINGLE scrtext_m FROM dd04t INTO lv_scrtext_m
      WHERE rollname   = lv_data_element
        AND ddlanguage = sy-langu
        AND as4local   = 'A'.

    IF sy-subrc = 0.
      cv_attr_name = lv_scrtext_m.
    ENDIF.
  ENDIF.

  CONCATENATE '"' cv_attr_name '"' INTO cv_attr_name.

ENDFORM.                    "get_attr_name
