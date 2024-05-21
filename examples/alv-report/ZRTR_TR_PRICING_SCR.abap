*----------------------------------------------------------------------*
*  INCLUDE ZRTR_TR_PRICING_SCR
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      FORM initialize
*&---------------------------------------------------------------------*
FORM initialize.

  DATA: lw_functxt TYPE smp_dyntxt.

  CLEAR lw_functxt.
  lw_functxt-icon_id    = icon_tools.
  lw_functxt-icon_text  = '######### (#### ############)'(005).
  sscrfields-functxt_01 = lw_functxt.

ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM at_selection_screen
*&---------------------------------------------------------------------*
FORM at_selection_screen.

  IF sscrfields-ucomm = 'FC01'.
    AUTHORITY-CHECK OBJECT 'S_TCODE'
            ID 'TCD' FIELD 'ZRTR_PT_SET'.
    IF sy-subrc = 0.
      CALL TRANSACTION 'ZRTR_PT_SET'.
    ELSE.
      MESSAGE e001(zrtrfa_msg) WITH '############ ##########!'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  file_dialog
*&---------------------------------------------------------------------*
FORM file_dialog CHANGING cv_path TYPE string.

  DATA: lv_num(5) TYPE n.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = ''
      initial_folder       = cv_path
    CHANGING
      selected_folder      = cv_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF cv_path IS NOT INITIAL.
    lv_num = strlen( cv_path ) - 1.
    IF cv_path+lv_num <> '\'.
      CONCATENATE cv_path '\' INTO cv_path.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR  'TB_0100'.

  PERFORM init_alv.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  EXIT_0100  INPUT
*&---------------------------------------------------------------------*
MODULE exit_0100 INPUT.

  save_ok = ok_code.
  CLEAR: ok_code.

  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  save_ok = ok_code.
  CLEAR: ok_code.

  gr_grid->check_changed_data( ).

  CASE save_ok.
    WHEN 'PRINT_01'. "############
      PERFORM create_form USING 'PRINT'
                                gc_print_form_doc
                                ''.
    WHEN 'PRINT_02'. "###########
      PERFORM create_form USING 'PRINT'
                                gc_print_form_rat
                                ''.
    WHEN 'XML_01'.  "############ (XML)
      PERFORM create_form USING 'XML'
                                ''
                                gc_xml_form_doc.
    WHEN 'XML_02'. "########### (XML)
      PERFORM create_form USING 'XML'
                                ''
                                gc_xml_form_rat.
  ENDCASE.

ENDMODULE.
