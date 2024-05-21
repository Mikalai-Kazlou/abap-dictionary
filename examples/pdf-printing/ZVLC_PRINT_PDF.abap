FUNCTION zvlc_print_pdf.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(IT_OUTPUT_CONTENT) TYPE  TABLE
*"     REFERENCE(IS_OUTPUT_PARAMS) TYPE  SFPOUTPUTPARAMS OPTIONAL
*"     REFERENCE(I_PDF_NAME) TYPE  FPNAME
*"  EXPORTING
*"     REFERENCE(EV_PDF) TYPE  FPCONTENT
*"----------------------------------------------------------------------
  TYPE-POOLS: abap.

  DATA:
    l_function_name TYPE funcname,
    l_languages(60) TYPE c,
    l_result        TYPE sfpjoboutput,
    l_lines         TYPE i.

  DATA:
    lw_outpar TYPE sfpoutputparams,
    lw_docpar TYPE sfpdocparams.

*--> 02.09.2015 LEVSHA
  DATA:
        ls_formoutput TYPE fpformoutput.
*<-- 02.09.2015 LEVSHA

* Check if the Russian language is installed
  CALL FUNCTION 'SYSTEM_INSTALLED_LANGUAGES'
    IMPORTING
      languages       = l_languages
    EXCEPTIONS
      sapgparam_error = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
    FIND FIRST OCCURRENCE OF 'R' IN l_languages.
    IF sy-subrc = 0.
      lw_docpar-langu = 'R'.
      lw_docpar-country = 'RU'.
    ENDIF.
  ENDIF.

* Get the name of the generated function module
  TRY.
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = i_pdf_name
        IMPORTING
          e_funcname = l_function_name.

      IF is_output_params IS NOT INITIAL.
        MOVE-CORRESPONDING: is_output_params TO lw_outpar.
      ELSE.
        lw_outpar-dest = z030_if_dfs_constants=>default_pdf_printer.
      ENDIF.

      CALL FUNCTION 'FP_JOB_OPEN'
        CHANGING
          ie_outputparams = lw_outpar
        EXCEPTIONS
          cancel          = 1
          usage_error     = 2
          system_error    = 3
          internal_error  = 4
          OTHERS          = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION l_function_name
        EXPORTING
          /1bcdwb/docparams  = lw_docpar
          edi_record         = it_output_content
        IMPORTING
          /1bcdwb/formoutput = ls_formoutput
        EXCEPTIONS
          usage_error        = 1
          system_error       = 2
          internal_error     = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
        CLEAR ev_pdf.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        ev_pdf = ls_formoutput-pdf.
      ENDIF.

*     Close the spool job
      CALL FUNCTION 'FP_JOB_CLOSE'
        IMPORTING
          e_result       = l_result
        EXCEPTIONS
          usage_error    = 1
          system_error   = 2
          internal_error = 3
          OTHERS         = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
  ENDTRY.

ENDFUNCTION.
