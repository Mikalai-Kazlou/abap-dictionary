FUNCTION z03_bt_mass_print.
*"--------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(IR_DATA) TYPE REF TO DATA
*"     REFERENCE(IT_ROWS) TYPE  LVC_T_ROW
*"--------------------------------------------------------------------

  INCLUDE zdf_find_card_030003_tab_def.

  DATA: ls_rows      LIKE LINE OF it_rows,
        lt_data_card TYPE TABLE OF zdf_card_030003,
        ls_data_card TYPE zdf_card_030003.

  FIELD-SYMBOLS: <lt_data> TYPE table,
                 <ls_data> TYPE tgs_data_030003.

  IF it_rows IS NOT INITIAL.
    ASSIGN ir_data->* TO <lt_data>.
    CHECK <lt_data> IS ASSIGNED.

    LOOP AT it_rows INTO ls_rows.
      READ TABLE <lt_data> ASSIGNING <ls_data> INDEX ls_rows-index.
      CHECK <ls_data> IS ASSIGNED.

      MOVE-CORRESPONDING: <ls_data> TO ls_data_card.
      APPEND ls_data_card TO lt_data_card.
    ENDLOOP.
  ELSE.
    " Не выбраны строки для печати!
    MESSAGE s026(z030_edi).
    EXIT.
  ENDIF.

  CALL FUNCTION 'ZEDI_MASS_PRINT'
    TABLES
      ct_card_data = lt_data_card.

ENDFUNCTION.
