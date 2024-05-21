CLASS ltcl_gdpr_obfuscation DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    CLASS-DATA:
      so_osql_test_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      prepare_osql_test_environment.

    DATA:
      mo_obfuscation_tool TYPE REF TO zcl_gdpr_obfuscation.

    METHODS:
      setup,

      _set_search_parameters FOR TESTING,
      _set_search_parameters_no_data FOR TESTING,

      _update_table FOR TESTING,
      _update_table_with_sql_error_1 FOR TESTING,
      _update_table_with_sql_error_2 FOR TESTING,

      _get_where_condition FOR TESTING,
      _get_where_condition_initial FOR TESTING,

      _get_extract_by_where_error FOR TESTING,
      _get_extract_by_class_error FOR TESTING.

ENDCLASS.

CLASS ltcl_gdpr_obfuscation IMPLEMENTATION.
  METHOD class_setup.
    prepare_osql_test_environment( ).
  ENDMETHOD.

  METHOD setup.
    mo_obfuscation_tool = NEW zcl_gdpr_obfuscation( ).
  ENDMETHOD.

  METHOD prepare_osql_test_environment.
    DATA: lt_zhrms_persinfo TYPE TABLE OF zhrms_persinfo.

    lt_zhrms_persinfo = VALUE #(
      ( employee_id               = '09999999999999999999'
        first_name                = 'John'
        last_name                 = 'Snow'
        business_email            = 'John_Snow@epam.com'
        persinfo_legal_first_name = 'John'
        persinfo_legal_last_name  = 'Snow'
        persinfo_native_full_name = 'John Snow' )
    ).

    so_osql_test_environment = cl_osql_test_environment=>create( VALUE #( ( 'ZHRMS_PERSINFO' ) ) ).
    so_osql_test_environment->insert_test_data( lt_zhrms_persinfo ).
  ENDMETHOD.

  METHOD _set_search_parameters.
    DATA(lv_field_1) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-employee_id.
    DATA(lv_field_2) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-first_name.
    DATA(lv_field_3) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-last_name.
    DATA(lv_field_4) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-full_name.
    DATA(lv_field_5) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-business_email.
    DATA(lv_field_6) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-persinfo_legal_first_name.
    DATA(lv_field_7) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-persinfo_legal_last_name.
    DATA(lv_field_8) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-persinfo_native_full_name.

    TRY.
        mo_obfuscation_tool->set_search_parameters( iv_employee_id = '09999999999999999999' ).
      CATCH zcx_gdpr_obfuscation.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

    DATA(lt_act) = mo_obfuscation_tool->mt_search_parameters.
    DATA(lt_exp) = VALUE zcl_gdpr_obfuscation=>tt_search_parameters(

      ( field = lv_field_1 value = '09999999999999999999' )
      ( field = lv_field_2 value = 'John' )
      ( field = lv_field_3 value = 'Snow' )
      ( field = lv_field_4 value = 'John Snow' )
      ( field = lv_field_5 value = 'John_Snow@epam.com' )
      ( field = lv_field_6 value = 'John' )
      ( field = lv_field_7 value = 'Snow' )
      ( field = lv_field_8 value = 'John Snow' )
    ).

    cl_abap_unit_assert=>assert_equals( act = lt_act
                                        exp = lt_exp ).
  ENDMETHOD.

  METHOD _set_search_parameters_no_data.
    TRY.
        mo_obfuscation_tool->set_search_parameters( iv_employee_id = '01111111111111111111' ).
      CATCH zcx_gdpr_obfuscation.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( ).
  ENDMETHOD.

  METHOD _update_table.
    DATA: lr_extract TYPE REF TO data,
          lt_act     TYPE TABLE OF zhrms_persinfo,
          lt_exp     TYPE TABLE OF zhrms_persinfo.

    SELECT * FROM zhrms_persinfo              "#EC CI_ALL_FIELDS_NEEDED
     WHERE employee_id = '09999999999999999999'
      INTO TABLE NEW @lr_extract.

    TRY.
        mo_obfuscation_tool->update_table(
          EXPORTING
            iv_table_name       = 'ZHRMS_PERSINFO'
            iv_table_field      = 'FIRST_NAME'
            iv_obfuscated_value = 'Employee'
            ir_extract          = lr_extract
        ).
      CATCH zcx_gdpr_obfuscation.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

    SELECT * FROM zhrms_persinfo
     WHERE employee_id = '09999999999999999999'
      INTO TABLE @lt_act.

    lt_exp = VALUE #(
      ( mandt                     = sy-mandt
        employee_id               = '09999999999999999999'
        first_name                = 'Employee'
        last_name                 = 'Snow'
        business_email            = 'John_Snow@epam.com'
        persinfo_legal_first_name = 'John'
        persinfo_legal_last_name  = 'Snow'
        persinfo_native_full_name = 'John Snow' )
    ).

    cl_abap_unit_assert=>assert_equals( act = lt_act
                                        exp = lt_exp ).
  ENDMETHOD.

  METHOD _update_table_with_sql_error_1.
    DATA: lr_extract TYPE REF TO data,
          lt_act     TYPE TABLE OF zhrms_persinfo,
          lt_exp     TYPE TABLE OF zhrms_persinfo.

    SELECT * FROM zhrms_persinfo              "#EC CI_ALL_FIELDS_NEEDED
     WHERE employee_id = '09999999999999999999'
      INTO TABLE NEW @lr_extract.

    TRY.
        mo_obfuscation_tool->update_table(
          EXPORTING
            iv_table_name       = '%TABLE_NOT_FOUND%'
            iv_table_field      = 'FIRST_NAME'
            iv_obfuscated_value = 'Employee'
            ir_extract          = lr_extract
        ).
      CATCH zcx_gdpr_obfuscation.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( ).
  ENDMETHOD.

  METHOD _update_table_with_sql_error_2.
    DATA: lr_extract TYPE REF TO data,
          lt_act     TYPE TABLE OF zhrms_persinfo,
          lt_exp     TYPE TABLE OF zhrms_persinfo.

    SELECT * FROM zhrms_persinfo              "#EC CI_ALL_FIELDS_NEEDED
     WHERE employee_id = '09999999999999999999'
      INTO TABLE NEW @lr_extract.

    TRY.
        mo_obfuscation_tool->update_table(
          EXPORTING
            iv_table_name       = 'ZHRMS_PERSINFO'
            iv_table_field      = '%FIELD_NOT_FOUND%'
            iv_obfuscated_value = 'Employee'
            ir_extract          = lr_extract
        ).
      CATCH zcx_gdpr_obfuscation.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( ).
  ENDMETHOD.

  METHOD _get_where_condition.

    DATA(lv_field_1) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-employee_id.
    DATA(lv_field_2) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-first_name.
    DATA(lv_field_3) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-last_name.
    DATA(lv_field_4) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-full_name.
    DATA(lv_field_5) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-business_email.
    DATA(lv_field_6) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-persinfo_legal_first_name.
    DATA(lv_field_7) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-persinfo_legal_last_name.
    DATA(lv_field_8) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-persinfo_native_full_name.

    mo_obfuscation_tool->mt_search_parameters = VALUE #(

      ( field = lv_field_1 value = '09999999999999999999' )
      ( field = lv_field_2 value = 'John' )
      ( field = lv_field_3 value = 'Snow' )
      ( field = lv_field_4 value = 'John Snow' )
      ( field = lv_field_5 value = 'John_Snow@epam.com' )
      ( field = lv_field_6 value = 'John (legal)' )
      ( field = lv_field_7 value = 'Snow (legal)' )
      ( field = lv_field_8 value = 'John Snow (native)' )
    ).

    DATA(ls_obfuscation_settings) = VALUE zcl_gdpr_obfuscation=>ts_obfuscation_settings_tab(
      extract_method-value = |{ lv_field_1 } = '<%{ lv_field_1 }%>'|
                     && | AND { lv_field_2 } = '<%{ lv_field_2 }%>'|
                     && | AND { lv_field_3 } = '<%{ lv_field_3 }%>'|
                     && | AND { lv_field_4 } = '<%{ lv_field_4 }%>'|
                     && | AND { lv_field_5 } = '<%{ lv_field_5 }%>'|
                     && | AND { lv_field_6 } = '<%{ lv_field_6 }%>'|
                     && | AND { lv_field_7 } = '<%{ lv_field_7 }%>'|
                     && | AND { lv_field_8 } = '<%{ lv_field_8 }%>'| ).
    TRY.
        DATA(lv_act) = mo_obfuscation_tool->get_where_condition( is_obfuscation_settings = ls_obfuscation_settings ).
      CATCH zcx_gdpr_obfuscation.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

    DATA(lv_exp) = |{ lv_field_1 } = '09999999999999999999'|
           && | AND { lv_field_2 } = 'John'|
           && | AND { lv_field_3 } = 'Snow'|
           && | AND { lv_field_4 } = 'John Snow'|
           && | AND { lv_field_5 } = 'John_Snow@epam.com'|
           && | AND { lv_field_6 } = 'John (legal)'|
           && | AND { lv_field_7 } = 'Snow (legal)'|
           && | AND { lv_field_8 } = 'John Snow (native)'|.

    cl_abap_unit_assert=>assert_equals( act = lv_act
                                        exp = lv_exp ).
  ENDMETHOD.

  METHOD _get_where_condition_initial.
    DATA(lv_field) = zcl_gdpr_obfuscation=>sc_obfuscation_fields-employee_id.

    mo_obfuscation_tool->mt_search_parameters = VALUE #(
      ( field = lv_field value = '' )
    ).

    DATA(ls_obfuscation_settings) = VALUE zcl_gdpr_obfuscation=>ts_obfuscation_settings_tab(
      extract_method-value = |{ lv_field } = '<%{ lv_field }%>'| ).
    TRY.
        DATA(lv_act) = mo_obfuscation_tool->get_where_condition( is_obfuscation_settings = ls_obfuscation_settings ).
      CATCH zcx_gdpr_obfuscation.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( ).
  ENDMETHOD.

  METHOD _get_extract_by_where_error.
    DATA(ls_obfuscation_settings) = VALUE zcl_gdpr_obfuscation=>ts_obfuscation_settings_tab(
      table_name = '%TABLE_NOT_FOUND%' ).
    TRY.
        DATA(lv_act) = mo_obfuscation_tool->get_extract_by_where( is_obfuscation_settings = ls_obfuscation_settings ).
      CATCH zcx_gdpr_obfuscation.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( ).
  ENDMETHOD.

  METHOD _get_extract_by_class_error.
    DATA(ls_obfuscation_settings) = VALUE zcl_gdpr_obfuscation=>ts_obfuscation_settings_tab(
      extract_method-value = '%NON-EXISTENT-CLASS%' ).
    TRY.
        DATA(lv_act) = mo_obfuscation_tool->get_extract_by_class( is_obfuscation_settings = ls_obfuscation_settings ).
      CATCH zcx_gdpr_obfuscation.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>fail( ).
  ENDMETHOD.
ENDCLASS.
