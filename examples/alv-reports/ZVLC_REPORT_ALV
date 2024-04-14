*&---------------------------------------------------------------------*
*&  Include           ZVLC_REPORT_ALV
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
        IMPORTING e_ucomm sender.

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
  ls_toolbar-quickinfo = 'Отметить все строки'.
  ls_toolbar-checked   = space.
  APPEND ls_toolbar TO e_object->mt_toolbar.

* Button 2
  CLEAR ls_toolbar.
  ls_toolbar-function  = 'UC_DESELECT_ALL'.                 "#EC NOTEXT
  ls_toolbar-icon      = icon_deselect_all.
  ls_toolbar-butn_type = '0'.
  ls_toolbar-disabled  = space.
  ls_toolbar-quickinfo = 'Внять отметку для всех строк'.
  ls_toolbar-checked   = space.
  APPEND ls_toolbar TO e_object->mt_toolbar.

ENDFORM.                    "handle_toolbar

*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
FORM handle_user_command USING e_ucomm LIKE sy-ucomm.

  FIELD-SYMBOLS: <fs_data> TYPE ts_data.

  CASE e_ucomm.
    WHEN 'UC_SELECT_ALL'.
      LOOP AT gt_data ASSIGNING <fs_data>.
        <fs_data>-zflag = abap_true.
      ENDLOOP.
    WHEN 'UC_DESELECT_ALL'.
      LOOP AT gt_data ASSIGNING <fs_data>.
        <fs_data>-zflag = abap_false.
      ENDLOOP.
  ENDCASE.

  PERFORM refresh_alv.

ENDFORM.                    "handle_user_command

*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
*       Initialization ALV-grid
*----------------------------------------------------------------------*
FORM init_alv.

  DATA: ls_variant TYPE disvariant.

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

    PERFORM build_fieldcat.

    gs_layout-cwidth_opt = abap_true.
    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    gs_layout-info_fname = 'ROWCOLOR'.

    CLEAR ls_variant.
    ls_variant-username = sy-uname.
    ls_variant-report   = sy-repid.
    ls_variant-handle   = 'HDR'.

    CLEAR: gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_info                  TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_graph                 TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_print                 TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_loc_cut               TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_loc_copy              TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_loc_paste             TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row     TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_loc_undo              TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_refresh               TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_check                 TO gt_excluding.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row        TO gt_excluding.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row          TO gt_excluding.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row        TO gt_excluding.
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row        TO gt_excluding.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row          TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_call_abc              TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_send                  TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_call_xxl              TO gt_excluding.
*    APPEND cl_gui_alv_grid=>mc_fc_views                 TO gt_excluding.

    SET HANDLER gr_grid->handle_toolbar               FOR gr_grid.
    SET HANDLER gr_grid->handle_user_command          FOR gr_grid.

    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = gt_excluding
        is_variant           = ls_variant
        is_layout            = gs_layout
        i_save               = 'A'
        i_default            = abap_true
      CHANGING
        it_outtab            = gt_data
        it_fieldcatalog      = gt_fieldcat.

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
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       Build fields catalog for ALV-grid
*----------------------------------------------------------------------*
FORM build_fieldcat.

  FIELD-SYMBOLS:
        <ls_fieldcat> TYPE lvc_s_fcat.

  CLEAR gt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'BSEG'
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
    ENDCASE.
  ENDLOOP.

ENDFORM.
