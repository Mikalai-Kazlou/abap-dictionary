*&---------------------------------------------------------------------*
*&  Include           ZEDI_DELIVERY_NOTE_DATA
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM get_dfs_data TABLES ct_items       STRUCTURE zdf_item_030003
                         ct_objectlinks STRUCTURE /dfs/drad
                         ct_mseg        STRUCTURE mseg
                         ct_ekpo        STRUCTURE ekpo
                   USING uv_vbeln  TYPE vbeln
                         is_mkpf   TYPE mkpf
                         is_ekko   TYPE ekko
                CHANGING cs_030003 TYPE zdf_card_030003.

  DATA: lv_bukrs TYPE bukrs.

  DATA: ls_ekpo TYPE ekpo,
        ls_mseg TYPE mseg.

  DATA: lv_sh_from_werks TYPE werks_d,
        lv_sh_from_lgort TYPE lgort_d,
        lv_sh_to_werks   TYPE werks_d,
        lv_sh_to_lgort   TYPE lgort_d.

*** Вид накладной
  CASE abap_on.
    WHEN rb_tn.
      cs_030003-delivery_type = gc_tn_type.
    WHEN rb_ttn.
      cs_030003-delivery_type = gc_ttn_type.
  ENDCASE.

*** Тип накладной
  cs_030003-invoice_type = '3'.

*** Валюта
  cs_030003-currency = z030_if_030003_constants=>local_currency.

*** Основание отпуска
  cs_030003-contract_name = 'Заказ на перемещение'.
  cs_030003-contract_id   = is_ekko-ebeln.
  cs_030003-contract_date = is_ekko-bedat.

*** Номер заказа
  cs_030003-order_id = is_ekko-ebeln.

*** Отправитель
  PERFORM get_bukrs USING uv_vbeln
                 CHANGING lv_bukrs.
  cs_030003-bukrs = lv_bukrs.

  CALL FUNCTION 'ZEDI_GET_BUKRS_DATA'
    EXPORTING
      iv_bukrs   = lv_bukrs
    CHANGING
      cv_gln     = cs_030003-shipper_gln
      cv_name    = cs_030003-shipper_name
      cv_address = cs_030003-shipper_address
      cv_vat_num = cs_030003-shipper_vat_num.

*** Получатель
  CASE cs_030003-invoice_type.
    WHEN '1'. " Исходящая накладная

    WHEN '3'. " Внутренняя накладная
      cs_030003-receiver_gln     = cs_030003-shipper_gln.
      cs_030003-receiver_name    = cs_030003-shipper_name.
      cs_030003-receiver_address = cs_030003-shipper_address.
      cs_030003-receiver_vat_num = cs_030003-shipper_vat_num.
  ENDCASE.

* Получим склады пуктов отгрузки / разгрузки
  READ TABLE ct_mseg INTO ls_mseg INDEX 1.
  IF sy-subrc = 0.
    lv_sh_from_werks = ls_mseg-werks.
    lv_sh_from_lgort = ls_mseg-lgort.

    READ TABLE ct_ekpo INTO ls_ekpo
      WITH KEY ebeln = ls_mseg-ebeln
               ebelp = ls_mseg-ebelp.

    IF sy-subrc = 0.
      lv_sh_to_werks = ls_ekpo-werks.
      lv_sh_to_lgort = ls_ekpo-lgort.
    ENDIF.
  ENDIF.

*** Пункт отгрузки
  cs_030003-sh_from_werks = lv_sh_from_werks.
  cs_030003-sh_from_lgort = lv_sh_from_lgort.

  CALL FUNCTION 'ZEDI_GET_LGORT_DATA'
    EXPORTING
      iv_date    = is_mkpf-budat
      iv_bukrs   = lv_bukrs
      iv_werks   = lv_sh_from_werks
      iv_lgort   = lv_sh_from_lgort
    CHANGING
      cv_gln     = cs_030003-sh_from_gln
      cv_address = cs_030003-sh_from_address
      cv_contact = cs_030003-shipper_contact.

  CALL FUNCTION 'ZEDI_GET_USER_DESCRIPTION'
    IMPORTING
      ev_description = cs_030003-sh_from_contact.

*** Пункт разгрузки
  cs_030003-sh_to_werks = lv_sh_to_werks.
  cs_030003-sh_to_lgort = lv_sh_to_lgort.

  CALL FUNCTION 'ZEDI_GET_LGORT_DATA'
    EXPORTING
      iv_date    = is_mkpf-budat
      iv_bukrs   = lv_bukrs
      iv_werks   = lv_sh_to_werks
      iv_lgort   = lv_sh_to_lgort
    CHANGING
      cv_gln     = cs_030003-sh_to_gln
      cv_address = cs_030003-sh_to_address
      cv_contact = cs_030003-sh_to_contact.

* Поля карточки, которые присутствуют только в ТТН
* -------------------------------------------------------------
  IF rb_ttn IS NOT INITIAL.
*** Заказчик перевозки
    CASE cs_030003-invoice_type.
      WHEN '1'. " Исходящая накладная

      WHEN '3'. " Внутренняя накладная
        cs_030003-fr_payer_gln     = cs_030003-shipper_gln.
        cs_030003-fr_payer_name    = cs_030003-shipper_name.
        cs_030003-fr_payer_address = cs_030003-shipper_address.
        cs_030003-fr_payer_vat_num = cs_030003-shipper_vat_num.

        cs_030003-x_fp_shipper = abap_true.
    ENDCASE.
  ENDIF.
* -------------------------------------------------------------

*** Товарные позиции
  PERFORM fill_items_data TABLES ct_items[]
                           USING ct_mseg[].

*** Ссылки на бизнес-объекты
  IF is_ekko-ebeln IS NOT INITIAL.
    ct_objectlinks-botype = 'Z_BUS2012'.                                " Заказ на поставку
    ct_objectlinks-bokey  = is_ekko-ebeln.
    APPEND ct_objectlinks.
  ENDIF.

  IF uv_vbeln IS NOT INITIAL.
    ct_objectlinks-botype = 'Z_LIKP'.                                   " Исходящая поставка
    ct_objectlinks-bokey  = uv_vbeln.
    APPEND ct_objectlinks.
  ENDIF.

  IF is_mkpf-mblnr IS NOT INITIAL AND is_mkpf-mjahr IS NOT INITIAL.
    ct_objectlinks-botype = 'Z_BUS2017'.                                " Документ материала
    CONCATENATE is_mkpf-mblnr is_mkpf-mjahr INTO ct_objectlinks-bokey.
    APPEND ct_objectlinks.
  ENDIF.

ENDFORM.                    "get_data

*&---------------------------------------------------------------------*
*&      Form  get_mm_data
*&---------------------------------------------------------------------*
FORM get_mm_data TABLES ct_mseg  STRUCTURE mseg
                        ct_ekpo  STRUCTURE ekpo
                  USING iv_vbeln TYPE vbeln
               CHANGING cs_mkpf  TYPE mkpf
                        cs_ekko  TYPE ekko.

  DATA: lv_ebeln TYPE ebeln,
        ls_mseg  TYPE mseg.

* Получим номер заказа на поставку
  SELECT SINGLE ebeln FROM ekbe INTO lv_ebeln
    WHERE belnr EQ iv_vbeln.

  IF sy-subrc EQ 0.
*   Получим заголовок заказа на поставку
    SELECT SINGLE * FROM ekko INTO cs_ekko
      WHERE ebeln EQ lv_ebeln.

*   Получим позиции заказа на поставку
    SELECT * FROM ekpo INTO TABLE ct_ekpo
      WHERE ebeln EQ lv_ebeln.
  ENDIF.

* Получим позиции документа материала
  IF lv_ebeln IS NOT INITIAL.
    PERFORM get_mseg_by_ebeln TABLES ct_mseg
                               USING iv_vbeln
                                     lv_ebeln.
  ENDIF.

* Получим заголовок документа материала
  IF ct_mseg[] IS NOT INITIAL.
    READ TABLE ct_mseg INTO ls_mseg INDEX 1.

    IF sy-subrc = 0.
      SELECT SINGLE * FROM mkpf INTO cs_mkpf
        WHERE mblnr EQ ls_mseg-mblnr
          AND mjahr EQ ls_mseg-mjahr.
    ENDIF.
  ENDIF.

ENDFORM.                    "get_mm_data

*&---------------------------------------------------------------------*
*&      Form  check_mm_data
*&---------------------------------------------------------------------*
FORM check_mm_data TABLES ct_mseg     STRUCTURE mseg
                          ct_ekpo     STRUCTURE ekpo
                          ct_msg      TYPE bal_t_msg
                    USING iv_vbeln    TYPE vbeln
                          is_mkpf     TYPE mkpf
                          is_ekko     TYPE ekko
                          if_show_msg TYPE xfeld
                 CHANGING cs_data     TYPE zedi_delivery_note_mass_list.

  DATA: ls_mseg        TYPE mseg,
        ls_ekpo        TYPE ekpo,
        ls_link        TYPE /dfs/rcgdocobl,
        ls_ttn_history TYPE zptp_ttn_history.

  DATA: lt_keyfields TYPE TABLE OF /dfs/str_keyfields,
        ls_keyfields TYPE /dfs/str_keyfields.

  DATA: ls_030003 TYPE zdf_card_030003,
        lv_status TYPE /dfs/dokst.

  DATA: lv_gln_lgort TYPE zedi_gln,
        rt_gln_user  TYPE zedi_rt_gln,
        lv_gln       TYPE zedi_gln,
        lv_bukrs     TYPE bukrs.

  DATA: lv_sh_from_werks TYPE werks_d,
        lv_sh_from_lgort TYPE lgort_d,
        lv_sh_to_werks   TYPE werks_d,
        lv_sh_to_lgort   TYPE lgort_d.

  DATA: lv_msg1 TYPE symsgv,
        lv_msg2 TYPE symsgv,
        lv_msg3 TYPE symsgv,
        lv_msg4 TYPE symsgv.

  DATA: lv_ok_answer TYPE c,
        lv_text      TYPE string.

* --------------------------------------------------------------------------------------------
*** Проверка на существование документа материала
* --------------------------------------------------------------------------------------------
  IF ct_mseg[] IS INITIAL.
*   Добавим сообщение
    lv_msg1 = 'Документ движения материала не найден!'.
    PERFORM add_msg USING 'E' 'Z030_EDI' '000' lv_msg1 '' '' '' CHANGING ct_msg[].

    IF if_show_msg IS NOT INITIAL.
      MESSAGE e000(z030_edi) WITH lv_msg1.
    ENDIF.
  ENDIF.

* --------------------------------------------------------------------------------------------
*** Проверка на внутреннее перемещение
* --------------------------------------------------------------------------------------------
  " Обрабатываем только двухшаговое перемещение (Склад -> ЦПиО), (ЦПиО -> Склад), (ЦПиО -> ЦПиО)
  IF is_ekko-bsart NE gc_bsart_ub.
*   Добавим сообщение
    lv_msg1 = 'Возможна обработка только внутреннего перемещения!'.
    PERFORM add_msg USING 'E' 'Z030_EDI' '000' lv_msg1 '' '' '' CHANGING ct_msg[].

    IF if_show_msg IS NOT INITIAL.
      MESSAGE e000(z030_edi) WITH lv_msg1.
    ENDIF.
  ENDIF.

* --------------------------------------------------------------------------------------------
*** Проверка на валюту (только BYR)
* --------------------------------------------------------------------------------------------
  IF ct_mseg[] IS NOT INITIAL.
    LOOP AT ct_mseg INTO ls_mseg WHERE waers NE gc_waers_byr.
    ENDLOOP.
    IF sy-subrc = 0.
*     Добавим сообщение
      lv_msg1 = 'Существуют строки документа с валютой отличной от'.
      lv_msg2 = gc_waers_byr.
      lv_msg3 = '!'.

      PERFORM add_msg USING 'E' 'Z030_EDI' '000' lv_msg1 lv_msg2 lv_msg3 '' CHANGING ct_msg[].

      IF if_show_msg IS NOT INITIAL.
        MESSAGE e000(z030_edi) WITH lv_msg1 lv_msg2 lv_msg3.
      ENDIF.
    ENDIF.
  ENDIF.

* --------------------------------------------------------------------------------------------
*** Проверка склада-получателя и склада-отправителя
* на возможность работы с электронными накладными
* --------------------------------------------------------------------------------------------
  READ TABLE ct_mseg INTO ls_mseg INDEX 1.
  IF sy-subrc = 0.
    lv_sh_from_werks = ls_mseg-werks.
    lv_sh_from_lgort = ls_mseg-lgort.

    READ TABLE ct_ekpo INTO ls_ekpo
      WITH KEY ebeln = ls_mseg-ebeln
               ebelp = ls_mseg-ebelp.

    IF sy-subrc = 0.
      lv_sh_to_werks = ls_ekpo-werks.
      lv_sh_to_lgort = ls_ekpo-lgort.
    ENDIF.

    PERFORM get_bukrs USING iv_vbeln
                   CHANGING lv_bukrs.

*   Склад-отправитель
    SELECT SINGLE gln FROM zedi_gln_lgort INTO lv_gln
     WHERE bukrs = lv_bukrs
       AND werks = lv_sh_from_werks
       AND lgort = lv_sh_from_lgort.

    IF sy-subrc NE 0.
      PERFORM add_msg USING 'W' 'Z030_EDI' '041' lv_sh_from_werks lv_sh_from_lgort '' '' CHANGING ct_msg[].

      IF if_show_msg IS NOT INITIAL.
        MESSAGE s041(z030_edi) WITH lv_sh_from_werks lv_sh_from_lgort INTO lv_text.
*       Зададим вопрос о возможности печати на бумажном бланке
        CONCATENATE lv_text cl_abap_char_utilities=>cr_lf
                    'Распечатать накладную на бумажном бланке?' INTO lv_text SEPARATED BY space.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = lv_text
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = lv_ok_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF sy-subrc <> 0 OR lv_ok_answer <> '1'.
          MESSAGE w041(z030_edi) WITH lv_sh_from_werks lv_sh_from_lgort.
        ELSE.
          PERFORM call_print_transaction USING iv_vbeln.
          LEAVE PROGRAM.
        ENDIF.
      ENDIF.
    ENDIF.

*   Склад-получатель
    SELECT SINGLE gln FROM zedi_gln_lgort INTO lv_gln
     WHERE bukrs = lv_bukrs
       AND werks = lv_sh_to_werks
       AND lgort = lv_sh_to_lgort.

    IF sy-subrc NE 0.
      PERFORM add_msg USING 'W' 'Z030_EDI' '041' lv_sh_to_werks lv_sh_to_lgort '' '' CHANGING ct_msg[].

      IF if_show_msg IS NOT INITIAL.
        MESSAGE s041(z030_edi) WITH lv_sh_to_werks lv_sh_to_lgort INTO lv_text.
*       Зададим вопрос о возможности печати на бумажном бланке
        CONCATENATE lv_text cl_abap_char_utilities=>cr_lf
                    'Распечатать накладную на бумажном бланке?' INTO lv_text SEPARATED BY space.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = lv_text
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = lv_ok_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF sy-subrc <> 0 OR lv_ok_answer <> '1'.
          MESSAGE w041(z030_edi) WITH lv_sh_to_werks lv_sh_to_lgort.
        ELSE.
          PERFORM call_print_transaction USING iv_vbeln.
          LEAVE PROGRAM.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* --------------------------------------------------------------------------------------------
*** Проверка соответствия GLN склада-отправителя и GLN пользователя
* --------------------------------------------------------------------------------------------
  READ TABLE ct_mseg INTO ls_mseg INDEX 1.
  IF sy-subrc = 0.
    CALL FUNCTION 'ZEDI_GET_LGORT_DATA'
      EXPORTING
        iv_bukrs = ls_mseg-bukrs
        iv_werks = ls_mseg-werks
        iv_lgort = ls_mseg-lgort
      CHANGING
        cv_gln   = lv_gln_lgort.

    CALL FUNCTION 'ZEDI_GET_USER_DATA'
      EXPORTING
        iv_uname = sy-uname
      CHANGING
        rt_gln   = rt_gln_user.

    IF rt_gln_user IS NOT INITIAL AND lv_gln_lgort IS NOT INITIAL.
      IF lv_gln_lgort NOT IN rt_gln_user.
*       Добавим сообщение
        lv_msg1 = 'GLN склада-отправителя'.
        lv_msg2 = lv_gln_lgort.
        lv_msg3 = 'не соответствует GLN пользователя!'.
        lv_msg4 = ''.

        PERFORM add_msg USING 'E' 'Z030_EDI' '000' lv_msg1 lv_msg2 lv_msg3 lv_msg4 CHANGING ct_msg[].

        IF if_show_msg IS NOT INITIAL.
          MESSAGE e000(z030_edi) WITH lv_msg1 lv_msg2 lv_msg3 lv_msg4.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* --------------------------------------------------------------------------------------------
*** Проверка на существование электронной накладной по данному документу
* --------------------------------------------------------------------------------------------
  ls_link-object = 'Z_BUS2017'. " Документ материала
  CONCATENATE is_mkpf-mblnr is_mkpf-mjahr INTO ls_link-objkey.

  " Получим существующие карточки, если они есть
  CALL FUNCTION '/DFS/GET_CARDS_BY_LINK'
    EXPORTING
      is_link          = ls_link
      i_ret_card_num   = '030003'
    TABLES
      et_keyfields     = lt_keyfields
    EXCEPTIONS
      error_input_data = 1
      OTHERS           = 2.

  IF sy-subrc = 0.
    LOOP AT lt_keyfields INTO ls_keyfields.
      " Получим данные карточки
      CALL FUNCTION 'ZDF_API_GET_CARD_INFO_030003'
        EXPORTING
          keyfield             = ls_keyfields
        IMPORTING
          status               = lv_status
          data_card            = ls_030003
        EXCEPTIONS
          card_not_found       = 1
          error_read_card_data = 2
          OTHERS               = 3.

      IF sy-subrc = 0.
        IF lv_status NE 'EX' AND  " Запрос отмены
           lv_status NE 'EY' AND  " Отмена принята
           lv_status NE 'EZ'.     " Отменен

          IF ls_030003-delete_flag IS INITIAL.
            cs_data-delivery_id = ls_030003-delivery_id.

*           Добавим сообщение
            lv_msg1 = 'Накладная по документу'.
            lv_msg2 = iv_vbeln.
            lv_msg3 = 'уже существует!'.
            CONCATENATE 'Номер:' ls_030003-delivery_id INTO lv_msg4 SEPARATED BY space.

            PERFORM add_msg USING 'W' 'Z030_EDI' '000' lv_msg1 lv_msg2 lv_msg3 lv_msg4 CHANGING ct_msg[].

            IF if_show_msg IS NOT INITIAL.
              MESSAGE w000(z030_edi) WITH lv_msg1 lv_msg2 lv_msg3 lv_msg4.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

* --------------------------------------------------------------------------------------------
*** Проверка на существование бумажной накладной по данному документу
* --------------------------------------------------------------------------------------------
  SELECT SINGLE * FROM zptp_ttn_history INTO ls_ttn_history
    WHERE iddoc = iv_vbeln
      AND status = 'P'.

  IF sy-subrc = 0.
    CONCATENATE ls_ttn_history-ttn_set ls_ttn_history-ttn_number INTO cs_data-delivery_id.

*   Добавим сообщение
    lv_msg1 = 'Накладная по документу'.
    lv_msg2 = iv_vbeln.
    lv_msg3 = 'уже существует!'.
    CONCATENATE 'Номер:' ls_ttn_history-ttn_set ls_ttn_history-ttn_number INTO lv_msg4 SEPARATED BY space.

    PERFORM add_msg USING 'W' 'Z030_EDI' '000' lv_msg1 lv_msg2 lv_msg3 lv_msg4 CHANGING ct_msg[].

    IF if_show_msg IS NOT INITIAL.
      MESSAGE w000(z030_edi) WITH lv_msg1 lv_msg2 lv_msg3 lv_msg4.
    ENDIF.
  ENDIF.

ENDFORM.                    "check_mm_data

*&---------------------------------------------------------------------*
*&      Form  select_mseg_by_ebeln
*&---------------------------------------------------------------------*
FORM get_mseg_by_ebeln TABLES ct_mseg STRUCTURE mseg
                        USING i_vbeln TYPE vbeln
                              i_ebeln TYPE ebeln.

  DATA: lt_vbfa     TYPE STANDARD TABLE OF vbfa,
        lw_vbfa     TYPE vbfa,
        lt_mseg     TYPE TABLE OF mseg,
        lt_mseg_add TYPE TABLE OF mseg,
        lt_mseg_del TYPE TABLE OF mseg,
        lw_mseg     TYPE mseg.

  DATA: lt_ekbe TYPE STANDARD TABLE OF ekbe.

  FIELD-SYMBOLS <fw_mseg> TYPE mseg.

  CLEAR ct_mseg.

  SELECT * FROM ekbe INTO CORRESPONDING FIELDS OF TABLE lt_ekbe
    WHERE ebeln EQ i_ebeln.

  IF sy-subrc = 0.
    SELECT *                                                "#EC *
    INTO CORRESPONDING FIELDS OF TABLE lt_mseg
      FROM mseg
      FOR ALL ENTRIES IN lt_ekbe
        WHERE mblnr EQ lt_ekbe-belnr
          AND mjahr EQ lt_ekbe-gjahr
          AND xauto NE abap_on
          AND bwart EQ gc_bwart_641.

    IF sy-subrc EQ 0 AND lt_mseg IS NOT INITIAL.
      " check for the reversing entry
      lt_mseg_del = lt_mseg.
      SORT lt_mseg_del ASCENDING BY mblnr gjahr.
      DELETE ADJACENT DUPLICATES FROM lt_mseg_del COMPARING mblnr gjahr.

      IF lt_mseg_del IS NOT INITIAL.
        SELECT DISTINCT mblnr gjahr smbln sjahr             "#EC *
        INTO CORRESPONDING FIELDS OF TABLE lt_mseg_del
          FROM mseg
          FOR ALL ENTRIES IN lt_mseg_del
            WHERE sjahr EQ lt_mseg_del-mjahr
              AND smbln EQ lt_mseg_del-mblnr.
        IF sy-subrc EQ 0.
          LOOP AT lt_mseg_del INTO lw_mseg.
            DELETE lt_mseg
              WHERE mblnr EQ lw_mseg-smbln
                AND gjahr EQ lw_mseg-sjahr.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF lt_mseg IS NOT INITIAL.
        CLEAR lt_mseg_add.
        SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_mseg_add
          FROM mseg
          FOR ALL ENTRIES IN lt_mseg
            WHERE mblnr EQ lt_mseg-mblnr
              AND gjahr EQ lt_mseg-gjahr
              AND xauto NE abap_on
              AND bwart EQ gc_bwart_641.
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.
      ENDIF.
    ELSE.
      RETURN.
    ENDIF.
  ENDIF.

  SELECT vbelv posnv vbeln posnn                            "#EC *
  INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
    FROM vbfa
      WHERE vbelv EQ i_vbeln.
  IF sy-subrc NE 0.
    CLEAR lt_vbfa.
  ENDIF.

  SORT lt_mseg_add ASCENDING BY mblnr gjahr zeile.
  DELETE ADJACENT DUPLICATES FROM lt_mseg_add COMPARING mblnr gjahr zeile.

  " check documents history
  LOOP AT lt_mseg_add ASSIGNING <fw_mseg>.
    READ TABLE lt_vbfa WITH KEY vbeln = <fw_mseg>-mblnr posnn = <fw_mseg>-zeile TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      <fw_mseg>-xauto = abap_on.
    ENDIF.
  ENDLOOP.

  DELETE lt_mseg_add WHERE xauto EQ abap_on.
  ct_mseg[] = lt_mseg_add[].

ENDFORM.                    "select_mseg_by_ebeln

*&---------------------------------------------------------------------*
*&      Form  fill_items_data
*&---------------------------------------------------------------------*
FORM fill_items_data TABLES ct_items STRUCTURE zdf_item_030003
                      USING it_mseg TYPE ty_t_mseg.

  DATA: ls_items TYPE zdf_item_030003.

  DATA: ls_mseg       TYPE mseg,
        ls_mbew       TYPE mbew,
        lt_mara_brgew TYPE STANDARD TABLE OF mara,
        ls_mara_brgew TYPE mara.

  DATA: lv_waers TYPE mseg-waers,
        lv_price TYPE mseg-dmbtr,
        lv_dmbtr TYPE mseg-dmbtr.

  DATA: lv_price_str(35) TYPE c,
        lv_dmbtr_str(35) TYPE c.

  DATA: lv_num(6)          TYPE n,
        lv_brgew           TYPE p DECIMALS 6,
        lv_matnr_output    TYPE matnr,
        lv_maktx_long(255) TYPE c.

* Получим таблицу c массой материалов
  IF it_mseg IS NOT INITIAL.
    SELECT matnr brgew gewei INTO CORRESPONDING FIELDS OF TABLE lt_mara_brgew
    FROM mara
      FOR ALL ENTRIES IN it_mseg
        WHERE matnr EQ it_mseg-matnr.
    IF sy-subrc NE 0.
      CLEAR lt_mara_brgew.
    ENDIF.
  ENDIF.

* Заполним таблицу товарных позиций
  LOOP AT it_mseg INTO ls_mseg.
    CLEAR: ls_items, ls_mbew.

*   Порядковый номер
    lv_num = lv_num + 1.
    ls_items-num = lv_num.

*   Вид товарной позиции
    ls_items-sign = 'PROD'.

*   Номер материала
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_mseg-matnr
      IMPORTING
        output = lv_matnr_output.

    ls_items-buyer_id    = lv_matnr_output.
    ls_items-supplier_id = lv_matnr_output.

*   Наименование материала
    PERFORM get_material_text USING ls_mseg-matnr
                                    ls_mseg-werks
                                    ls_mseg-charg
                           CHANGING lv_maktx_long.

    ls_items-name = lv_maktx_long.

*   Количество
    ls_items-quan_despatched = ls_mseg-menge.

*   Суммы
    IF ls_mseg-menge IS NOT INITIAL.
      lv_waers = ls_mseg-waers.

      " Стоимость партии
      lv_dmbtr = ls_mseg-dmbtr.

      " Среднескользящая стоимость партии
      IF lv_dmbtr IS INITIAL.
        lv_dmbtr = ls_mseg-salk3 * ls_mseg-menge / ls_mseg-lbkum.
      ENDIF.

      " Среднескользящая стоимость по заводу
      IF lv_dmbtr IS INITIAL.
        SELECT SINGLE * FROM mbew INTO ls_mbew
          WHERE matnr = ls_mseg-matnr
            AND bwkey = ls_mseg-werks
            AND bwtar = ''.

        IF sy-subrc = 0.
          lv_dmbtr = ls_mbew-salk3 * ls_mseg-menge / ls_mbew-lbkum.
        ENDIF.
      ENDIF.

      " Цена
      lv_price = lv_dmbtr / ls_mseg-menge.

      " Если не смогли определить цену
      IF lv_price IS INITIAL.
        lv_price = 1 / 100.
        lv_dmbtr = lv_price * ls_mseg-menge.
      ENDIF.

      ls_items-price = lv_price.
      ls_items-amount_wo_charge = lv_dmbtr.
      ls_items-amount = lv_dmbtr.
    ENDIF.

*   Единица измерения
    SELECT SINGLE isocode FROM t006 INTO ls_items-quan_uom
      WHERE msehi = ls_mseg-meins.

    IF ls_items-quan_uom NE 'CMK' OR ls_items-quan_uom NE 'CMQ' OR ls_items-quan_uom NE 'CMT' OR ls_items-quan_uom NE 'DMQ' OR
       ls_items-quan_uom NE 'GRM' OR ls_items-quan_uom NE 'KGM' OR ls_items-quan_uom NE 'LTR' OR ls_items-quan_uom NE 'MGM' OR
       ls_items-quan_uom NE 'MLT' OR ls_items-quan_uom NE 'MMT' OR ls_items-quan_uom NE 'MTK' OR ls_items-quan_uom NE 'MTQ' OR
       ls_items-quan_uom NE 'MTR' OR ls_items-quan_uom NE 'PCE' OR ls_items-quan_uom NE 'PR'  OR ls_items-quan_uom NE 'TNE'.

      ls_items-quan_uom = 'PCE'.
    ENDIF.

*   Страна производства
    PERFORM read_country_by_batch USING ls_mseg-matnr
                                        ls_mseg-werks
                                        ls_mseg-charg
                               CHANGING ls_items-country_origin.

* Поля карточки, которые присутствуют только в ТТН
* -------------------------------------------------------------
    IF rb_ttn IS NOT INITIAL.

*     Масса
      READ TABLE lt_mara_brgew INTO ls_mara_brgew WITH KEY matnr = ls_mseg-matnr.
      IF sy-subrc EQ 0.
        IF ls_mara_brgew-brgew IS NOT INITIAL AND ls_mara_brgew-gewei IS NOT INITIAL.

          lv_brgew = ls_mara_brgew-brgew * ls_mseg-menge.
          CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
            EXPORTING
              input                = lv_brgew
              unit_in              = ls_mara_brgew-gewei
              unit_out             = 'TO'
            IMPORTING
              output               = lv_brgew
            EXCEPTIONS
              conversion_not_found = 1
              division_by_zero     = 2
              input_invalid        = 3
              output_invalid       = 4
              overflow             = 5
              type_invalid         = 6
              units_missing        = 7
              unit_in_not_found    = 8
              unit_out_not_found   = 9
              OTHERS               = 10.
          IF sy-subrc EQ 0.
            ls_items-gross_weight_val = lv_brgew.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.
* -------------------------------------------------------------

    APPEND ls_items TO ct_items.
  ENDLOOP.

ENDFORM.                    "fill_items_data

*&---------------------------------------------------------------------*
*&      Form  get_material_text
*&---------------------------------------------------------------------*
FORM get_material_text USING VALUE(i_matnr) TYPE mseg-matnr
                             VALUE(i_werks) TYPE mseg-werks
                             VALUE(i_charg) TYPE mseg-charg
                    CHANGING e_maktx.

  DATA lv_tdname TYPE tdobname.
  DATA lt_lines TYPE STANDARD TABLE OF tline.
  DATA lw_lines TYPE tline.
  DATA lv_maktx  TYPE makt-maktx.

  CLEAR e_maktx.

* Long name from CHARG
  CONCATENATE i_matnr i_werks i_charg INTO lv_tdname RESPECTING BLANKS.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = gc_id
      language                = gc_lang
      name                    = lv_tdname
      object                  = gc_object
    TABLES
      lines                   = lt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc EQ 0 AND lt_lines[] IS NOT INITIAL.
    LOOP AT lt_lines INTO lw_lines.
      CONCATENATE e_maktx lw_lines-tdline
      INTO e_maktx SEPARATED BY space.
    ENDLOOP.
  ENDIF.

* Long name from MATNR
  IF e_maktx IS INITIAL.
    lv_tdname = i_matnr.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = gc_id_material
        language                = gc_lang
        name                    = lv_tdname
        object                  = gc_object_material
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc EQ 0 AND lt_lines[] IS NOT INITIAL.
      LOOP AT lt_lines INTO lw_lines.
        CONCATENATE e_maktx lw_lines-tdline
        INTO e_maktx SEPARATED BY space.
      ENDLOOP.
    ENDIF.
  ENDIF.

* Material name
  IF e_maktx IS INITIAL.
    SELECT SINGLE maktx FROM makt INTO lv_maktx
      WHERE matnr EQ i_matnr
        AND spras EQ gc_lang.
    IF sy-subrc EQ 0.
      e_maktx = lv_maktx.
    ENDIF.
  ENDIF.

  CONDENSE e_maktx.

ENDFORM.                    "get_material_text

*&---------------------------------------------------------------------*
*&      Form  read_country_by_batch
*&---------------------------------------------------------------------*
FORM read_country_by_batch USING i_matnr   TYPE lips-matnr
                                 i_werks   TYPE lips-werks
                                 i_charg   TYPE lips-charg
                        CHANGING e_country TYPE zedi_li_country_of_origin.

  DATA l_objecttable      TYPE bapi1003_key-objecttable.
  DATA l_classnum         TYPE bapi1003_key-classnum.
  DATA l_classtype        TYPE bapi1003_key-classtype.
  DATA l_language         TYPE bapifieldscacl-bapilangua.
  DATA lt_allocvaluesnum  TYPE STANDARD TABLE OF bapi1003_alloc_values_num.
  DATA lt_allocvalueschar TYPE STANDARD TABLE OF bapi1003_alloc_values_char.
  DATA lw_allocvalueschar TYPE bapi1003_alloc_values_char.
  DATA lt_allocvaluescurr TYPE STANDARD TABLE OF bapi1003_alloc_values_curr.
  DATA l_objectkey        TYPE bapi1003_key-object.
  DATA lt_return          TYPE STANDARD TABLE OF bapiret2.

  l_objecttable = gc_objecttable.
  l_classnum    = gc_classnum.
  l_classtype   = gc_classtype.
  l_language    = gc_lang.

  CONCATENATE i_matnr i_werks i_charg INTO l_objectkey RESPECTING BLANKS.

  REFRESH: lt_allocvaluesnum,
           lt_allocvalueschar,
           lt_allocvaluescurr,
           lt_return.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey       = l_objectkey
      objecttable     = l_objecttable
      classnum        = l_classnum
      classtype       = l_classtype
      language        = l_language
    TABLES
      allocvaluesnum  = lt_allocvaluesnum
      allocvalueschar = lt_allocvalueschar
      allocvaluescurr = lt_allocvaluescurr
      return          = lt_return.

  IF lt_allocvalueschar[] IS NOT INITIAL.
    " Страна происхождения
    READ TABLE lt_allocvalueschar INTO lw_allocvalueschar
      WITH KEY charact = gc_charact_country_origin.
    IF sy-subrc EQ 0.
      e_country = lw_allocvalueschar-value_char.
    ELSE.
      " Страна ввоза
      READ TABLE lt_allocvalueschar INTO lw_allocvalueschar
        WITH KEY charact = gc_charact_country_import.
      IF sy-subrc EQ 0.
        e_country = lw_allocvalueschar-value_char.
      ENDIF.
    ENDIF.
  ELSE.
    e_country = gc_country_by.
  ENDIF.

ENDFORM.                    "read_country_by_batch

*&---------------------------------------------------------------------*
*&      Form  get_bukrs
*&---------------------------------------------------------------------*
FORM get_bukrs USING uv_vbeln TYPE vbeln
            CHANGING cv_bukrs TYPE bukrs.

  DATA: lv_bukrs TYPE bukrs.

  " select the company code by the delivery document
  SELECT SINGLE bukrs INTO lv_bukrs FROM t001k
    INNER JOIN lips ON lips~werks EQ t001k~bwkey
      WHERE vbeln EQ uv_vbeln.

  IF lv_bukrs IS INITIAL.
    MESSAGE i001(00) WITH 'Company code is not found!'.
  ELSE.
    cv_bukrs = lv_bukrs.
  ENDIF.

ENDFORM.                    "get_bukrs

*&---------------------------------------------------------------------*
*&      Form  call_print_transaction
*&---------------------------------------------------------------------*
FORM call_print_transaction USING iv_vbeln TYPE vbeln.

  CASE abap_on.
    WHEN rb_tn.
      SUBMIT zptpmm_ttn_config VIA SELECTION-SCREEN
        WITH p_vbeln = iv_vbeln
        WITH p_r_tn  = 'X'
        WITH p_r_ttn = ' '
        AND RETURN.
    WHEN rb_ttn.
      SUBMIT zptpmm_ttn_config VIA SELECTION-SCREEN
        WITH p_vbeln = iv_vbeln
        WITH p_r_tn  = ' '
        WITH p_r_ttn = 'X'
        AND RETURN.
  ENDCASE.

ENDFORM.                    "call_print_transaction
