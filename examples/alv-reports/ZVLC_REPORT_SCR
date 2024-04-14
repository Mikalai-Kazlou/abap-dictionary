*----------------------------------------------------------------------*
*  INCLUDE ZVLC_REPORT_SCR
*----------------------------------------------------------------------*

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
    WHEN 'PRINT_01'. "Печатная форма
      PERFORM print_oxml USING 'ZTMP'.
  ENDCASE.

ENDMODULE.
