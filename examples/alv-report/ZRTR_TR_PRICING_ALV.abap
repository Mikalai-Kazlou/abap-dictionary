*&---------------------------------------------------------------------*
*&  Include           ZRTR_TR_PRICING_ALV
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&       Class lcl_alv_grid
*&---------------------------------------------------------------------*
CLASS lcl_alv_grid DEFINITION INHERITING FROM cl_gui_alv_grid.

  PUBLIC SECTION.
    METHODS:

      constructor
        IMPORTING i_parent TYPE REF TO cl_gui_container,

*     Handle for toolbar event
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

*     Handle for user command event
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm sender,

*     Handle for hotspot click event
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

ENDCLASS.

*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_alv_grid
*&---------------------------------------------------------------------*
CLASS lcl_alv_grid IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        i_parent = i_parent.
  ENDMETHOD.                    "constructor

  METHOD handle_toolbar.
    PERFORM handle_toolbar USING e_object e_interactive.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.
    PERFORM handle_user_command USING e_ucomm.
  ENDMETHOD.                    "handle_user_command

  METHOD handle_hotspot_click.
    PERFORM hotspot_click USING e_row_id e_column_id.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.               "lcl_alv_grid

*&---------------------------------------------------------------------*
*&      Form  handle_toolbar
*&---------------------------------------------------------------------*
FORM handle_toolbar USING e_object TYPE REF TO cl_alv_event_toolbar_set
                          e_interactive TYPE c.

  DATA: ls_toolbar TYPE stb_button.

* Seperator
  ls_toolbar-function  = 'DUMMY'.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO e_object->mt_toolbar.

* Button 1
  CLEAR ls_toolbar.
  ls_toolbar-function  = 'UC_SELECT_ALL'.                   "#EC NOTEXT
  ls_toolbar-icon      = icon_select_all.
  ls_toolbar-butn_type = '0'.
  ls_toolbar-disabled  = space.
  ls_toolbar-quickinfo = '########'.
  ls_toolbar-checked   = space.
  APPEND ls_toolbar TO e_object->mt_toolbar.

* Button 2
  CLEAR ls_toolbar.
  ls_toolbar-function  = 'UC_DESELECT_ALL'.                 "#EC NOTEXT
  ls_toolbar-icon      = icon_deselect_all.
  ls_toolbar-butn_type = '0'.
  ls_toolbar-disabled  = space.
  ls_toolbar-quickinfo = 'C#### #######'.
  ls_toolbar-checked   = space.
  APPEND ls_toolbar TO e_object->mt_toolbar.

ENDFORM.                    "handle_toolbar

*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
FORM handle_user_command USING e_ucomm LIKE sy-ucomm.

  DATA: lt_row_indexes TYPE lvc_t_row WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_data> TYPE ts_data.

  " ####### ########## ######
  CALL METHOD gr_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_row_indexes[].

  DELETE lt_row_indexes WHERE rowtype <> ''.

  CASE e_ucomm.
    WHEN 'UC_SELECT_ALL'.
      IF lines( lt_row_indexes ) > 0.
        LOOP AT lt_row_indexes.
          READ TABLE gt_data ASSIGNING <fs_data> INDEX lt_row_indexes-index.
          IF sy-subrc = 0.
            <fs_data>-zflag = abap_true.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT gt_data ASSIGNING <fs_data>.
          <fs_data>-zflag = abap_true.
        ENDLOOP.
      ENDIF.
    WHEN 'UC_DESELECT_ALL'.
      IF lines( lt_row_indexes ) > 0.
        LOOP AT lt_row_indexes.
          READ TABLE gt_data ASSIGNING <fs_data> INDEX lt_row_indexes-index.
          IF sy-subrc = 0.
            <fs_data>-zflag = abap_false.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT gt_data ASSIGNING <fs_data>.
          <fs_data>-zflag = abap_false.
        ENDLOOP.
      ENDIF.
  ENDCASE.

  PERFORM refresh_alv.

ENDFORM.                    "handle_user_command

*&---------------------------------------------------------------------*
*&      Form  hotspot_click
*&---------------------------------------------------------------------*
FORM hotspot_click USING e_row TYPE lvc_s_row
                         e_col TYPE lvc_s_col.

  DATA: ls_data TYPE ts_data.

  CHECK e_row-rowtype IS INITIAL.

  READ TABLE gt_data INDEX e_row-index INTO ls_data.
  IF sy-subrc = 0.
    CASE e_col-fieldname.
      WHEN 'BELNR'.
        SET PARAMETER ID 'BLN' FIELD ls_data-belnr.
        SET PARAMETER ID 'BUK' FIELD ls_data-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_data-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDIF.

ENDFORM.                    "hotspot_click

*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
*       Initialization ALV-grid
*----------------------------------------------------------------------*
FORM init_alv.

  IF gr_grid IS NOT BOUND.
    IF gr_ext_docking IS NOT BOUND.
      CREATE OBJECT gr_docking_empty
        EXPORTING
          side      = cl_gui_docking_container=>dock_at_left
          extension = cl_gui_docking_container=>ws_minimizebox
          repid     = sy-repid
          dynnr     = sy-dynnr.

      gr_ext_docking ?= gr_docking_empty.
    ENDIF.

    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = gr_ext_docking.

    PERFORM prepare_fieldcat.
    PERFORM prepare_sorting.

    CLEAR: gs_layout.
    gs_layout-cwidth_opt = abap_true.
    gs_layout-sel_mode   = 'A'.

    CLEAR: gs_variant.
    gs_variant-username = sy-uname.
    gs_variant-report   = sy-repid.
    gs_variant-handle   = 'HDR'.

    CLEAR: gt_excluding.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row        TO gt_excluding.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row          TO gt_excluding.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row        TO gt_excluding.
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row        TO gt_excluding.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row          TO gt_excluding.

    SET HANDLER gr_grid->handle_toolbar       FOR gr_grid.
    SET HANDLER gr_grid->handle_user_command  FOR gr_grid.
    SET HANDLER gr_grid->handle_hotspot_click FOR gr_grid.

    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = gt_excluding
        is_variant           = gs_variant
        is_layout            = gs_layout
        i_save               = 'A'
        i_default            = abap_true
      CHANGING
        it_outtab            = gt_data
        it_fieldcatalog      = gt_fieldcat
        it_sort              = gt_sorting[].

  ELSE.
    PERFORM refresh_alv.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
FORM refresh_alv.
  DATA: ls_is_stable TYPE lvc_s_stbl.

  ls_is_stable-row = abap_true.
  ls_is_stable-col = abap_true.

  CALL METHOD gr_grid->refresh_table_display
    EXPORTING
      is_stable = ls_is_stable.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_FIELDCAT
*&---------------------------------------------------------------------*
*       Build fields catalog for ALV-grid
*----------------------------------------------------------------------*
FORM prepare_fieldcat.

  FIELD-SYMBOLS:
        <ls_fieldcat> TYPE lvc_s_fcat.

  CLEAR: gt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZRTR_TR_PRICING_ALV'
      i_bypassing_buffer     = abap_true
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    CLEAR gt_fieldcat.
  ENDIF.

  LOOP AT gt_fieldcat ASSIGNING <ls_fieldcat>.
    CASE <ls_fieldcat>-fieldname.
      WHEN 'ZFLAG'.
        <ls_fieldcat>-edit      = abap_true.
        <ls_fieldcat>-checkbox  = abap_true.
        <ls_fieldcat>-outputlen = 3.
      WHEN 'BELNR'.
        <ls_fieldcat>-hotspot = abap_true.
      WHEN 'WRBTR'.
        <ls_fieldcat>-do_sum = abap_true.
      WHEN 'DMBTR'.
        <ls_fieldcat>-do_sum = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  prepare_sorting
*&---------------------------------------------------------------------*
FORM prepare_sorting.

  DATA: ls_sorting TYPE lvc_s_sort.

  CLEAR: gt_sorting.

  CLEAR ls_sorting.
  ls_sorting-spos = 1.
  ls_sorting-fieldname = 'LIFNR'.
  ls_sorting-up = 'X'.
  APPEND ls_sorting TO gt_sorting.

  CLEAR ls_sorting.
  ls_sorting-spos = 2.
  ls_sorting-fieldname = 'KUNNR'.
  ls_sorting-up = 'X'.
  APPEND ls_sorting TO gt_sorting.

  CLEAR ls_sorting.
  ls_sorting-spos = 3.
  ls_sorting-fieldname = 'VERTN'.
  ls_sorting-up = 'X'.
  ls_sorting-subtot = 'X'.
  APPEND ls_sorting TO gt_sorting.

  CLEAR ls_sorting.
  ls_sorting-spos = 4.
  ls_sorting-fieldname = 'MATNR'.
  ls_sorting-up = 'X'.
  APPEND ls_sorting TO gt_sorting.

  CLEAR ls_sorting.
  ls_sorting-spos = 5.
  ls_sorting-fieldname = 'BUDAT'.
  ls_sorting-up = 'X'.
  APPEND ls_sorting TO gt_sorting.

  CLEAR ls_sorting.
  ls_sorting-spos = 6.
  ls_sorting-fieldname = 'BLDAT'.
  ls_sorting-up = 'X'.
  APPEND ls_sorting TO gt_sorting.

ENDFORM.                    "prepare_sorting
