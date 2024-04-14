*----------------------------------------------------------------------*
***INCLUDE ZEDI_DELIVERY_NOTE_ALV .
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
*   Handle for double click event
    handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row
                e_column,
*   Handle for respond button click event
    respond_button_click
      FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING es_col_id
                es_row_no.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_double_click.
    PERFORM double_click USING e_row e_column.
  ENDMETHOD.                    "handle_double_click

  METHOD respond_button_click.
    PERFORM respond_button_click USING es_col_id es_row_no.
  ENDMETHOD.                    "respond_button_click

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  double_click
*&---------------------------------------------------------------------*
FORM double_click USING e_row TYPE lvc_s_row e_col TYPE lvc_s_col.

  CHECK e_row-rowtype IS INITIAL.
  READ TABLE gt_data INDEX e_row-index INTO gs_data.
  IF sy-subrc = 0.
    CASE e_col-fieldname.
      WHEN 'EXCPT'.
        PERFORM show_msg_table TABLES gs_data-msglg
                             CHANGING gv_log_handle.
    ENDCASE.
  ENDIF.

ENDFORM.                    "double_click

*&---------------------------------------------------------------------*
*&      Form  respond_button_click
*&---------------------------------------------------------------------*
FORM respond_button_click USING es_col_id TYPE lvc_s_col
                                es_row_no TYPE lvc_s_roid.

  DATA: ls_ttn_history TYPE zptp_ttn_history.

  READ TABLE gt_data INDEX es_row_no-row_id INTO gs_data.
  IF sy-subrc = 0.
    CASE es_col_id-fieldname.
      WHEN 'BT_PRINT'.
*       Печать накладной на бланке
        PERFORM call_print_transaction USING gs_data-vbeln.

*       Поиск номера бумажного бланка среди распечатанных накладных
        SELECT SINGLE * FROM zptp_ttn_history INTO ls_ttn_history
          WHERE iddoc = gs_data-vbeln
            AND status = 'P'.

        IF sy-subrc = 0.
          REFRESH gs_data-style.
          CLEAR gs_data-bt_print.

          CONCATENATE ls_ttn_history-ttn_set ls_ttn_history-ttn_number INTO gs_data-delivery_id.
          gs_data-excpt = 3.

          MODIFY gt_data FROM gs_data INDEX es_row_no-row_id.
          PERFORM refresh_table.
        ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    "respond_button_click

*&---------------------------------------------------------------------*
*&      Form  show_table
*&---------------------------------------------------------------------*
FORM show_table.
  DATA: ls_variant TYPE disvariant.

  IF gr_container IS NOT BOUND.
    CREATE OBJECT gr_container
      EXPORTING
        container_name = 'ZSCR_001'.

    CREATE OBJECT gr_alv
      EXPORTING
        i_parent = gr_container.

    PERFORM prepare_fcatalog.
    PERFORM prepare_layout.
    PERFORM prepare_sorting.
    PERFORM prepare_excluding.

    PERFORM register_events.
    ls_variant-report = sy-repid.

    CALL METHOD gr_alv->set_table_for_first_display
      EXPORTING
        is_variant           = ls_variant
        i_save               = 'A'
        is_layout            = gs_layout
        it_toolbar_excluding = gt_toolbar_excluding
      CHANGING
        it_fieldcatalog      = gt_fcatalog[]
        it_outtab            = gt_data[]
        it_sort              = gt_sorting[].
  ELSE.
    PERFORM refresh_table.
  ENDIF.

ENDFORM.                    "show_table

*&---------------------------------------------------------------------*
*&      Form  refresh_table
*&---------------------------------------------------------------------*
FORM refresh_table.

  DATA: ls_stable TYPE lvc_s_stbl.

  ls_stable-row = 'X'.
  ls_stable-col = 'X'.
  CALL METHOD gr_alv->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

ENDFORM.                    "refresh_table

*&---------------------------------------------------------------------*
*&      Form  create_fcatalog
*&---------------------------------------------------------------------*
FORM prepare_fcatalog.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = gc_alv_structure
    CHANGING
      ct_fieldcat      = gt_fcatalog[].

  LOOP AT gt_fcatalog.
    CASE gt_fcatalog-fieldname.
      WHEN 'EXCPT'.
        gt_fcatalog-coltext = 'Статус'.
        gt_fcatalog-tooltip = 'Статус'.
      WHEN 'GROSS_WEIGHT_VAL'.
        CASE abap_true.
          WHEN rb_tn.
            gt_fcatalog-no_out = abap_true.
          WHEN rb_ttn.
            gt_fcatalog-edit = 'X'.
            gt_fcatalog-outputlen = 15.
            gt_fcatalog-no_out = abap_false.
        ENDCASE.
      WHEN 'QUAN_UNIT'.
        CASE abap_true.
          WHEN rb_tn.
            gt_fcatalog-no_out = abap_true.
          WHEN rb_ttn.
            gt_fcatalog-edit = 'X'.
            gt_fcatalog-outputlen = 20.
            gt_fcatalog-no_out = abap_false.
        ENDCASE.
      WHEN 'DELIVERY_ID'.
        gt_fcatalog-outputlen = 30.
      WHEN 'BT_PRINT'.
        gt_fcatalog-coltext = 'Печать БСО'.
        gt_fcatalog-tooltip = 'Печать на бланке строгой отчетности'.
        gt_fcatalog-outputlen = 10.
    ENDCASE.
    MODIFY gt_fcatalog.
  ENDLOOP.

ENDFORM.                    " create_fcatalog

*&---------------------------------------------------------------------*
*&      Form  prepare_layout
*&---------------------------------------------------------------------*
*    Layout for main screen
*----------------------------------------------------------------------*
FORM prepare_layout.

  gs_layout-sel_mode   = 'A'.
  gs_layout-excp_fname = 'EXCPT'.
  gs_layout-stylefname = 'STYLE'.
  gs_layout-zebra      = 'X'.
  gs_layout-no_merging = 'X'.

ENDFORM.                    " prepare_layout

*&---------------------------------------------------------------------*
*&      Form  prepare_sorting
*&---------------------------------------------------------------------*
FORM prepare_sorting.

  CLEAR gt_sorting.
  gt_sorting-spos = 1.
  gt_sorting-fieldname = 'VBELN'.
  APPEND gt_sorting.

ENDFORM.                    "prepare_sorting

*&---------------------------------------------------------------------*
*&      Form  prepare_excluding
*&---------------------------------------------------------------------*
FORM prepare_excluding.

  APPEND cl_gui_alv_grid=>mc_mb_paste          TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_graph          TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_info           TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_check          TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_refresh        TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut        TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy       TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo       TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO gt_toolbar_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row   TO gt_toolbar_excluding.

*  APPEND cl_gui_alv_grid=>mc_mb_variant        TO gt_toolbar_excluding.
*  APPEND cl_gui_alv_grid=>mc_mb_filter         TO gt_toolbar_excluding.
*  APPEND cl_gui_alv_grid=>mc_mb_sum            TO gt_toolbar_excluding.
*  APPEND cl_gui_alv_grid=>mc_mb_export         TO gt_toolbar_excluding.
*  APPEND cl_gui_alv_grid=>mc_mb_view           TO gt_toolbar_excluding.
*  APPEND cl_gui_alv_grid=>mc_fc_print          TO gt_toolbar_excluding.
*  APPEND cl_gui_alv_grid=>mc_fc_find           TO gt_toolbar_excluding.
*  APPEND cl_gui_alv_grid=>mc_fc_detail         TO gt_toolbar_excluding.
*  APPEND cl_gui_alv_grid=>mc_fg_sort           TO gt_toolbar_excluding.

ENDFORM.                    "prepare_excluding

*&---------------------------------------------------------------------*
*&      Form  register_events
*&---------------------------------------------------------------------*
FORM register_events.

  DATA: gref_event_receiver TYPE REF TO lcl_event_receiver.

  CALL METHOD gr_alv->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD gr_alv->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT gref_event_receiver.
  SET HANDLER gref_event_receiver->handle_double_click  FOR gr_alv.
  SET HANDLER gref_event_receiver->respond_button_click FOR gr_alv.

ENDFORM.                    "register_events
