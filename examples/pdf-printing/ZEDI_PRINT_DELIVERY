FUNCTION zedi_print_delivery.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(IS_OUTPUT_PARAMS) TYPE  SFPOUTPUTPARAMS OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_PDF) TYPE  FPCONTENT
*"  TABLES
*"      CT_CARD_DATA STRUCTURE  ZDF_CARD_030003
*"      TAB_ITEM_030003 TYPE  ZDF_TAB_ITEM_030003 OPTIONAL
*"----------------------------------------------------------------------
  TYPES: type_controls_table_enh TYPE /dfs/descr_alv.

  DATA: ls_card_data TYPE zdf_card_030003,
        lv_pdf_name  TYPE char30.

  DATA: lt_rows_print      TYPE zedi_030003_tprint,
        lw_rows_print      TYPE zedi_030003_sprint,
        lt_tab_item        TYPE zdf_tab_item_030003,
        lw_str_item        TYPE zdf_item_030003,
        lt_total_line_item TYPE zdf_tab_item_030003,
        ls_total_line_item TYPE zdf_item_030003.

  DATA: lv_delivery_date_str(10) TYPE c,
        lv_landx                 TYPE landx.

  FIELD-SYMBOLS: <fs_str_item> TYPE zdf_item_030003.

  LOOP AT ct_card_data INTO ls_card_data.
    IF lw_rows_print IS INITIAL.
      MOVE-CORRESPONDING: ls_card_data TO lw_rows_print.
      lw_rows_print-registry_date = sy-datum.

      CALL FUNCTION 'ZEDI_GET_USER_DESCRIPTION'
        EXPORTING
          iv_uname = ls_card_data-created_by
          iv_date  = ls_card_data-delivery_date
        IMPORTING
          ev_iof   = lw_rows_print-sh_from_contact.

      WRITE ls_card_data-delivery_date TO lv_delivery_date_str DD/MM/YYYY.
      CONCATENATE ls_card_data-delivery_id 'от' lv_delivery_date_str 'г.'
        INTO lw_rows_print-delivery_descr SEPARATED BY space.

      CONCATENATE ls_card_data-shipper_vat_num  ';' ls_card_data-shipper_name  ';' ls_card_data-shipper_address  INTO lw_rows_print-shipper_descr.
      CONCATENATE ls_card_data-receiver_vat_num ';' ls_card_data-receiver_name ';' ls_card_data-receiver_address INTO lw_rows_print-receiver_descr.
      CONCATENATE ls_card_data-fr_payer_vat_num ';' ls_card_data-fr_payer_name ';' ls_card_data-fr_payer_address INTO lw_rows_print-fr_payer_descr.

      CONCATENATE 'ZEDI_030003_PRINT_' ls_card_data-delivery_type '_VRT' INTO lv_pdf_name.
    ENDIF.

*   Таблица товарных позиций
    IF tab_item_030003[] IS NOT INITIAL.
      lt_tab_item[] = tab_item_030003[].
    ELSE.
      SELECT * FROM zdf_item_030003 INTO TABLE lt_tab_item
        WHERE dokar = ls_card_data-dokar
          AND doknr = ls_card_data-doknr
          AND dokvr = ls_card_data-dokvr
          AND doktl = ls_card_data-doktl.

      LOOP AT lt_tab_item INTO lw_str_item.
        ls_total_line_item-amount_wo_charge = lw_str_item-amount_wo_charge.
        ls_total_line_item-amount_charge    = lw_str_item-amount_charge.
        ls_total_line_item-amount           = lw_str_item-amount.
        ls_total_line_item-quan_despatched  = lw_str_item-quan_despatched.
        ls_total_line_item-gross_weight_val = lw_str_item-gross_weight_val.
        ls_total_line_item-quan_unit        = lw_str_item-quan_unit.
        ls_total_line_item-price            = lw_str_item-price.
        COLLECT ls_total_line_item INTO lt_total_line_item.
      ENDLOOP.

      READ TABLE lt_total_line_item INTO ls_total_line_item INDEX 1.
      IF sy-subrc = 0.
        CLEAR: lw_str_item.

        lw_str_item-name             = 'ИТОГО:'.
        lw_str_item-amount_wo_charge = ls_total_line_item-amount_wo_charge.
        lw_str_item-amount_charge    = ls_total_line_item-amount_charge.
        lw_str_item-amount           = ls_total_line_item-amount.
        lw_str_item-quan_despatched  = ls_total_line_item-quan_despatched.
        lw_str_item-gross_weight_val = ls_total_line_item-gross_weight_val.
        lw_str_item-quan_unit        = ls_total_line_item-quan_unit.
        lw_str_item-price            = ls_total_line_item-price.
        APPEND lw_str_item TO lt_tab_item.
      ENDIF.
    ENDIF.

*   Страна производства в наименовании
    LOOP AT lt_tab_item ASSIGNING <fs_str_item>.
      IF <fs_str_item>-country_origin IS NOT INITIAL.
        SELECT SINGLE landx FROM t005t INTO lv_landx
          WHERE spras = 'R'
            AND land1 = <fs_str_item>-country_origin.
        IF sy-subrc = 0.
          CONCATENATE <fs_str_item>-name ',' INTO <fs_str_item>-name.
          CONCATENATE <fs_str_item>-name lv_landx INTO <fs_str_item>-name SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.

    lw_rows_print-tab_item = lt_tab_item.
    APPEND lw_rows_print TO lt_rows_print.
  ENDLOOP.

  CALL FUNCTION 'ZVLC_PRINT_PDF'
    EXPORTING
      it_output_content = lt_rows_print
      is_output_params  = is_output_params
      i_pdf_name        = lv_pdf_name
    IMPORTING
      ev_pdf            = ev_pdf.

ENDFUNCTION.
