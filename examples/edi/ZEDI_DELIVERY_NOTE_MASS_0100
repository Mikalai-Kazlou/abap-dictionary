*----------------------------------------------------------------------*
*  INCLUDE ZEDI_DELIVERY_NOTE_MASS_0100.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR  'TB_0100'.

  IF rb_tn IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name CS 'TRANSPORT_OWNER'
      OR screen-name CS 'TRANSPORT_ID'
      OR screen-name CS 'TRAILER_ID'
      OR screen-name CS 'WAYBILL_ID'
      OR screen-name CS 'DRIVER'
      OR screen-name CS 'QUANTITY_TRIP'.

        screen-input     = 0.
        screen-output    = 0.
        screen-invisible = 1.

        MODIFY SCREEN.
      ENDIF.

      IF screen-name CS 'PROXY_ID'
      OR screen-name CS 'PROXY_DATE'
      OR screen-name CS 'PROXY_ORG_NAME'.

        screen-required = 0.

        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: lv_valid TYPE c.

  ok_code_save = ok_code.
  CLEAR: ok_code.

  CASE ok_code_save.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'CREATE'.
*     Обновим данные таблицы
      CALL METHOD gr_alv->check_changed_data
        IMPORTING
          e_valid = lv_valid.

*     Проверка введенных данных
      PERFORM check_input_data.

*     Запуск процесса создания карточек
      PERFORM process_data.
    WHEN 'COPY'.
*     Скопируем данные из другой карточки
      PERFORM copy_data.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  INITIALIZATION_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE initialization_0100 OUTPUT.

* Покажем таблицу данных
  PERFORM show_table.

ENDMODULE.                 " INITIALIZATION_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  check_input_data
*&---------------------------------------------------------------------*
FORM check_input_data.

  DATA: lt_row_indexes     TYPE lvc_t_row WITH HEADER LINE,
        lv_field_name(100).

  FIELD-SYMBOLS: <fs>.

* Проверим заполнение обязательных полей на экране
  LOOP AT SCREEN.
    IF screen-name CS 'ZDF_CARD_030003'.
      IF screen-input = 1 AND screen-required = 2.

        CONCATENATE '(' sy-cprog ')' screen-name INTO lv_field_name.
        ASSIGN (lv_field_name) TO <fs>.
        IF sy-subrc = 0.
          IF <fs> IS INITIAL.
            MESSAGE e000(z030_edi) WITH 'Заполните все обязательные поля!'.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.

* Проверим заполнение обязательных полей в таблице
  IF rb_ttn IS NOT INITIAL.
*   Получим выделенные строки
    CALL METHOD gr_alv->get_selected_rows
      IMPORTING
        et_index_rows = lt_row_indexes[].

    LOOP AT lt_row_indexes WHERE rowtype IS INITIAL.
      READ TABLE gt_data INTO gs_data INDEX lt_row_indexes-index.
      IF sy-subrc = 0.

        IF gs_data-gross_weight_val IS INITIAL.
          MESSAGE e000(z030_edi) WITH 'Проверьте заполнение колонки' '"Масса позиции"!'.
        ENDIF.
        IF gs_data-quan_unit IS INITIAL.
          MESSAGE e000(z030_edi) WITH 'Проверьте заполнение колонки' '"Кол-во грузовых мест"!'.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "check_input_data

*&---------------------------------------------------------------------*
*&      Form  copy_data
*&---------------------------------------------------------------------*
FORM copy_data.

  DATA: lt_card_keys   TYPE /dfs/str_keyfields OCCURS 0 WITH HEADER LINE,
        ls_card_keys   TYPE /dfs/str_keyfields,
        ls_030003      TYPE zdf_card_030003,
        lt_cardd       TYPE /dfs/cardd OCCURS 0 WITH HEADER LINE,
        lt_copy_fields TYPE TABLE OF zedi_copy_fields,
        ls_copy_fields TYPE zedi_copy_fields,
        ls_data        TYPE zedi_delivery_note_mass_list,
        lv_dn_type     TYPE zedi_delivery_note_type,
        lrt_doknr      TYPE RANGE OF /dfs/doknr WITH HEADER LINE,
        lv_werks       TYPE werks_d,
        lv_lgort       TYPE lgort_d,
        lv_gln         TYPE zedi_gln,
        lv_ucomm       TYPE sy-ucomm.

  FIELD-SYMBOLS: <fs_value> TYPE any.

* --> 18.12.2018 (ITCR-22316)
* Определим GLN, с которого осуществляется отгрузка
  READ TABLE gt_data INTO ls_data INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE werks lgort FROM lips INTO ( lv_werks, lv_lgort )
      WHERE vbeln = ls_data-vbeln.
    IF sy-subrc = 0.
      SELECT SINGLE gln FROM zedi_gln_lgort INTO lv_gln
        WHERE werks = lv_werks
          AND lgort = lv_lgort.
    ENDIF.
  ENDIF.

* Выбираем последние n накладных, отправленных с данного GLN-номера
  SELECT dokar doknr dokvr doktl INTO CORRESPONDING FIELDS OF TABLE lt_card_keys
    FROM zdf_card_030003 UP TO 200 ROWS
    WHERE sh_from_gln EQ lv_gln
      AND delete_flag NE 'X'
    ORDER BY doknr DESCENDING.

  IF lt_card_keys[] IS INITIAL.
    EXIT.
  ENDIF.

  lrt_doknr-sign = 'I'.
  lrt_doknr-option = 'EQ'.
  LOOP AT lt_card_keys INTO ls_card_keys.
    lrt_doknr-low = ls_card_keys-doknr.
    APPEND lrt_doknr.
  ENDLOOP.

* Запустим поисковый отчет и получим ключ выбранной карточки
  SUBMIT zdf_find_card_030003
    WITH so_doknr IN lrt_doknr
    WITH p_dc_fn = 'ZEDI_CARD_LIST_GRID'
    AND RETURN.

  CLEAR: ls_card_keys.
  IMPORT ls_card_keys FROM MEMORY ID z030_if_030003_constants=>memid_exp_card_keys.
  FREE MEMORY ID z030_if_030003_constants=>memid_exp_card_keys.

  IF ls_card_keys IS INITIAL.
    EXIT.
  ENDIF.

  CASE abap_true.
    WHEN rb_tn.  lv_dn_type = z030_if_030003_constants=>tn.
    WHEN rb_ttn. lv_dn_type = z030_if_030003_constants=>ttn.
  ENDCASE.

* Покажем окно для выбора копируемых полей
  CALL FUNCTION 'ZEDI_POPUP_COPY_FIELDS'
    EXPORTING
      is_keyfields         = ls_card_keys
      iv_for_delivery_type = lv_dn_type
    TABLES
      ct_copy_fields       = lt_copy_fields.

* Скопируем отмеченные поля из выбранной карточки
  LOOP AT lt_copy_fields INTO ls_copy_fields WHERE fieldmark = 'X'.
    ASSIGN COMPONENT ls_copy_fields-fieldname OF STRUCTURE zdf_card_030003 TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CASE ls_copy_fields-fieldtype.
        WHEN 'D'.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external            = ls_copy_fields-fieldvalue
              accept_initial_date      = 'X'
            IMPORTING
              date_internal            = <fs_value>
            EXCEPTIONS
              date_external_is_invalid = 1
              OTHERS                   = 2.
        WHEN OTHERS.
          <fs_value> = ls_copy_fields-fieldvalue.
      ENDCASE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "copy_data
