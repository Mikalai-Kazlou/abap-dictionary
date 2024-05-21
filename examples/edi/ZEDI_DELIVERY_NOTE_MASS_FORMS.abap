*----------------------------------------------------------------------*
*   INCLUDE ZEDI_DELIVERY_NOTE_MASS_FORMS.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
FORM process_data.

  DATA: lt_row_indexes TYPE lvc_t_row WITH HEADER LINE,
        lt_messages TYPE TABLE OF bdcmsgcoll,
        lw_messages TYPE bdcmsgcoll,
        lw_style    TYPE lvc_s_styl.

  DATA: lv_tabix     TYPE int4,
        lv_total     TYPE int4,
        lv_index(10) TYPE c,
        lv_ok_answer TYPE c,
        lv_text      TYPE string.

  FIELD-SYMBOLS: <ls_data> LIKE LINE OF gt_data.

* Получим выделенные строки
  CALL METHOD gr_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_row_indexes[].

  DELETE lt_row_indexes[] WHERE rowtype <> ''.

  lv_total = LINES( lt_row_indexes[] ).
  IF lv_total = 0.
    MESSAGE w012(zptpmm_reserv_reval).
*   'Выберите хотя бы одну строку из списка'
    RETURN.
  ENDIF.

* Зададим вопрос о необходимости создания карточке
  lv_text = lv_total.
  CONCATENATE 'Создать карточки накладных (' lv_text ' шт. ) ?' INTO lv_text SEPARATED BY space.

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
    RETURN.
  ENDIF.

*** СОЗДАНИЕ КАРТОЧЕК
************************************************************************
  LOOP AT lt_row_indexes WHERE rowtype IS INITIAL.
    lv_tabix = sy-tabix.

    READ TABLE gt_data INTO gs_data INDEX lt_row_indexes-index.
    IF gs_data-excpt = 3.
      CONTINUE.
    ENDIF.

    REFRESH:
      gt_ekpo, gt_objectlinks,
      gt_mseg, gt_items,
      gt_msg,  gs_data-msglg.

    CLEAR:
      gs_ekko, gs_030003,
      gs_mkpf.

*   Заполним даные доверенности и транспорта
    MOVE-CORRESPONDING: zdf_card_030003 TO gs_030003.

*   Получим ММ-данные
    PERFORM get_mm_data TABLES gt_mseg gt_ekpo
                         USING gs_data-vbeln
                      CHANGING gs_mkpf gs_ekko.

*   Первичные проверки данных
    PERFORM check_mm_data TABLES gt_mseg gt_ekpo gt_msg
                           USING gs_data-vbeln gs_mkpf gs_ekko abap_false
                        CHANGING gs_data.

*   Проверим таблицу ошибок
    IF LINES( gt_msg ) > 0.
      PERFORM get_excpt USING gt_msg CHANGING gs_data-excpt.
      gs_data-msglg = gt_msg.

      MODIFY gt_data FROM gs_data INDEX lt_row_indexes-index.
      CONTINUE.
    ENDIF.

*   Получим данные для создания карточки
    PERFORM get_dfs_data TABLES gt_items gt_objectlinks gt_mseg gt_ekpo
                          USING gs_data-vbeln gs_mkpf gs_ekko
                       CHANGING gs_030003.

    IF rb_ttn IS NOT INITIAL.
*     Распределим итоговые данные товарных позиций (кол-во грузовых мест)
      PERFORM distribute_places TABLES gt_items
                                 USING gs_data.

*     Распределим итоговые данные товарных позиций (масса)
      PERFORM distribute_weight TABLES gt_items
                                 USING gs_data.
    ENDIF.

*   Проверим данные перед созданием карточки
    PERFORM check_dfs_data TABLES gt_items gt_msg
                            USING gs_030003.

*   Проверим таблицу ошибок
    IF LINES( gt_msg ) > 0.
      PERFORM get_excpt USING gt_msg CHANGING gs_data-excpt.
      gs_data-msglg = gt_msg.

      MODIFY gt_data FROM gs_data INDEX lt_row_indexes-index.
      CONTINUE.
    ENDIF.

*   Создадим карточку
    PERFORM create_card TABLES gt_items gt_objectlinks gt_msg
                         USING gs_030003 abap_false.

*   Проверим таблицу ошибок
    IF LINES( gt_msg ) > 0.
      PERFORM get_excpt USING gt_msg CHANGING gs_data-excpt.
      gs_data-msglg = gt_msg.

      MODIFY gt_data FROM gs_data INDEX lt_row_indexes-index.
      CONTINUE.
    ELSE.
      gs_data-excpt = 3.
      gs_data-delivery_id = gs_030003-delivery_id.
      MODIFY gt_data FROM gs_data INDEX lt_row_indexes-index.
    ENDIF.

    PERFORM progress_indicator USING 'Создание карточки' lv_tabix lv_total.
  ENDLOOP.
************************************************************************

* Установка кнопок печати накладной на бумажном бланке
  CLEAR lw_style.
  lw_style-fieldname = 'BT_PRINT'.
  lw_style-style = cl_gui_alv_grid=>mc_style_button.

  LOOP AT lt_row_indexes WHERE rowtype IS INITIAL.
    lv_tabix = sy-tabix.

    READ TABLE gt_data INTO gs_data INDEX lt_row_indexes-index.
    IF sy-subrc = 0.
      REFRESH gs_data-style.
      CLEAR gs_data-bt_print.

      IF gs_data-excpt = 1 OR ( gs_data-excpt = 2 AND gs_data-delivery_id IS INITIAL ).
        APPEND lw_style TO gs_data-style.
        gs_data-bt_print = icon_print.
      ENDIF.

      MODIFY gt_data FROM gs_data INDEX lt_row_indexes-index.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  get_excpt
*&---------------------------------------------------------------------*
FORM get_excpt USING it_msg   TYPE bal_t_msg
            CHANGING cv_excpt TYPE zvlc_status.

  DATA: ls_msg   TYPE bal_s_msg,
        lv_excpt TYPE zvlc_status.

  cv_excpt = 3.

  LOOP AT it_msg INTO ls_msg.
    CASE ls_msg-msgty.
      WHEN 'A' OR 'X' OR 'E'.
        lv_excpt = 1.
      WHEN 'W'.
        lv_excpt = 2.
      WHEN 'I' OR 'S'.
        lv_excpt = 3.
    ENDCASE.

    IF lv_excpt < cv_excpt.
      cv_excpt = lv_excpt.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "get_excpt

*&---------------------------------------------------------------------*
*&      Form  distribute_places
*&---------------------------------------------------------------------*
FORM distribute_places TABLES ct_items STRUCTURE zdf_item_030003
                        USING is_data TYPE zedi_delivery_note_mass_list.

  DATA: lv_total      TYPE zedi_li_quantity_despatched,
        lv_sum_places TYPE zedi_li_despatch_unit_quantity,
        lv_dif_places TYPE zedi_li_despatch_unit_quantity,
        lv_max_places TYPE zedi_li_despatch_unit_quantity.

  DATA: lv_tabix TYPE sy-tabix.

  FIELD-SYMBOLS: <fs_s_item> TYPE zdf_item_030003.

* Посчитаем общее количество
  LOOP AT ct_items ASSIGNING <fs_s_item>.
    lv_total = lv_total + <fs_s_item>-quan_despatched.
  ENDLOOP.

* Распределим общее кол-во грузовых мест
  LOOP AT ct_items ASSIGNING <fs_s_item>.
    <fs_s_item>-quan_unit = <fs_s_item>-quan_despatched * is_data-quan_unit / lv_total.

    " Найдем строку с максимальным кол-вом грузовых мест
    IF lv_max_places < <fs_s_item>-quan_unit.
      lv_max_places = <fs_s_item>-quan_unit.
      lv_tabix = sy-tabix.
    ENDIF.

    lv_sum_places = lv_sum_places + <fs_s_item>-quan_unit.
  ENDLOOP.

* Скорректируем кол-во грузовых мест в строке с максимальным значением, если это нужно
  lv_dif_places = lv_sum_places - is_data-quan_unit.
  IF lv_dif_places <> 0.
    READ TABLE ct_items ASSIGNING <fs_s_item> INDEX lv_tabix.
    IF sy-subrc EQ 0 AND <fs_s_item> IS ASSIGNED.
      IF <fs_s_item>-quan_unit > lv_dif_places.
        <fs_s_item>-quan_unit = <fs_s_item>-quan_unit - lv_dif_places.
      ENDIF.
    ENDIF.
  ENDIF.

* Скорректируем кол-во грузовых мест с нулевым значением
  LOOP AT ct_items ASSIGNING <fs_s_item>.
    IF <fs_s_item>-quan_unit IS INITIAL.
      <fs_s_item>-quan_unit = 1 / 100.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "distribute_places

*&---------------------------------------------------------------------*
*&      Form  distribute_weight
*&---------------------------------------------------------------------*
FORM distribute_weight TABLES ct_items STRUCTURE zdf_item_030003
                        USING is_data TYPE zedi_delivery_note_mass_list.

  DATA: lv_total      TYPE zedi_li_quantity_despatched,
        lv_sum_weight TYPE zedi_li_gross_weight_value,
        lv_dif_weight TYPE zedi_li_gross_weight_value,
        lv_max_weight TYPE zedi_li_gross_weight_value.

  DATA: lv_tabix TYPE sy-tabix.

  FIELD-SYMBOLS: <fs_s_item> TYPE zdf_item_030003.

* Посчитаем общее количество
  LOOP AT ct_items ASSIGNING <fs_s_item>.
    lv_total = lv_total + <fs_s_item>-quan_despatched.
  ENDLOOP.

* Распределим общую массу
  LOOP AT ct_items ASSIGNING <fs_s_item>.
    <fs_s_item>-gross_weight_val = <fs_s_item>-quan_despatched * is_data-gross_weight_val / lv_total.

    " Найдем строку с максимальной массой
    IF lv_max_weight < <fs_s_item>-gross_weight_val.
      lv_max_weight = <fs_s_item>-gross_weight_val.
      lv_tabix = sy-tabix.
    ENDIF.

    lv_sum_weight = lv_sum_weight + <fs_s_item>-gross_weight_val.
  ENDLOOP.

* Скорректируем массу в строке с максимальным значением, если это нужно
  lv_dif_weight = lv_sum_weight - is_data-gross_weight_val.
  IF lv_dif_weight <> 0.
    READ TABLE ct_items ASSIGNING <fs_s_item> INDEX lv_tabix.
    IF sy-subrc EQ 0 AND <fs_s_item> IS ASSIGNED.
      IF <fs_s_item>-gross_weight_val > lv_dif_weight.
        <fs_s_item>-gross_weight_val = <fs_s_item>-gross_weight_val - lv_dif_weight.
      ENDIF.
    ENDIF.
  ENDIF.

* Скорректируем массу с нулевым значением
  LOOP AT ct_items ASSIGNING <fs_s_item>.
    IF <fs_s_item>-gross_weight_val IS INITIAL.
      <fs_s_item>-gross_weight_val = 1 / 1000000.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "distribute_weight

*&---------------------------------------------------------------------*
*&      Form  progress_indicator
*&---------------------------------------------------------------------*
FORM progress_indicator USING p_string p_tabix TYPE int4 p_total TYPE int4.

  DATA: lv_text(100),
        lv_tabix_str(20),
        lv_total_str(20),
        lv_percentage TYPE i.

  WRITE p_string TO lv_text.

  IF p_tabix IS NOT INITIAL AND p_total IS NOT INITIAL.
    lv_percentage = ( p_tabix * 100 ) / p_total.

    WRITE p_tabix TO lv_tabix_str LEFT-JUSTIFIED NO-ZERO.
    WRITE p_total TO lv_total_str LEFT-JUSTIFIED NO-ZERO.
    CONCATENATE lv_text lv_tabix_str '/' lv_total_str INTO lv_text SEPARATED BY space.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lv_percentage
      text       = lv_text
    EXCEPTIONS
      OTHERS     = 1.

ENDFORM.                    "progress_indicator
