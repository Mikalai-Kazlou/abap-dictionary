*&---------------------------------------------------------------------*
*& Include          ZTMP_TR_COPY_CLS
*&---------------------------------------------------------------------*

CLASS lcl_tr_processor DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      create
        IMPORTING iv_astext        TYPE e07t-as4text
                  iv_trfunc        TYPE e070-trfunction
                  iv_cltsrc        TYPE e070c-client
                  iv_tarsys        TYPE e070-tarsystem
        RETURNING VALUE(rv_trkorr) TYPE e070-trkorr,

      copy_objects
        IMPORTING iv_trkorr_fr   TYPE e070-trkorr
                  iv_trkorr_to   TYPE e070-trkorr
        RETURNING VALUE(rv_fail) TYPE abap_bool,


      release
        IMPORTING iv_trkorr      TYPE e070-trkorr
        RETURNING VALUE(rv_fail) TYPE abap_bool.

ENDCLASS.

CLASS lcl_tr_processor IMPLEMENTATION.
  METHOD create.

    "WRITE: / '>>>>> Copy of TR was not created'.
    "WRITE: / '>>>>> Copy of TR was created'.
    "rv_trkorr = 'FS2K999999'.

    CALL FUNCTION 'TR_INSERT_NEW_COMM'
      EXPORTING
        wi_kurztext             = iv_astext
        wi_trfunction           = iv_trfunc
        iv_tarsystem            = iv_tarsys
        wi_client               = iv_cltsrc
      IMPORTING
        we_trkorr               = rv_trkorr
      EXCEPTIONS
        client_range_full       = 1
        e070l_insert_error      = 2
        e070l_update_error      = 3
        e070_insert_error       = 4
        e07t_insert_error       = 5
        e070c_insert_error      = 6
        e070m_insert_error      = 7
        no_systemname           = 8
        no_systemtype           = 9
        sap_range_full          = 10
        unallowed_trfunction    = 11
        unallowed_user          = 12
        order_not_found         = 13
        invalid_targetsystem    = 14
        invalid_target_devclass = 15
        invalid_devclass        = 16
        invalid_target_layer    = 17
        invalid_status          = 18
        not_an_order            = 19
        order_lock_failed       = 20
        no_authorization        = 21
        wrong_client            = 22
        file_access_error       = 23
        wrong_category          = 24
        internal_error          = 25
        OTHERS                  = 26.

    IF sy-subrc <> 0.
      CLEAR: rv_trkorr.
    ENDIF.
  ENDMETHOD.

  METHOD copy_objects.

    "WRITE: / '>>>>> Objects were copied'.
    "WRITE: / '>>>>> Objects were not copied'.
    "rv_fail = abap_true.

    CALL FUNCTION 'TR_COPY_COMM'
      EXPORTING
        wi_trkorr_from           = iv_trkorr_fr
        wi_trkorr_to             = iv_trkorr_to
        wi_without_documentation = space
      EXCEPTIONS
        db_access_error          = 1
        trkorr_from_not_exist    = 2
        trkorr_to_is_repair      = 3
        trkorr_to_locked         = 4
        trkorr_to_not_exist      = 5
        trkorr_to_released       = 6
        user_not_owner           = 7
        no_authorization         = 8
        wrong_client             = 9
        wrong_category           = 10
        object_not_patchable     = 11
        OTHERS                   = 12.

    IF sy-subrc <> 0.
      rv_fail = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD release.

    "WRITE: / '>>>>> TR of copy was released'.
    "WRITE: / '>>>>> TR of copy was not released'.
    "rv_fail = abap_true.

    CALL FUNCTION 'TR_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                  = iv_trkorr
        iv_display_export_log      = abap_true
      EXCEPTIONS
        cts_initialization_failure = 1
        enqueue_failed             = 2
        no_authorization           = 3
        invalid_request            = 4
        request_already_released   = 5
        repeat_too_early           = 6
        object_check_error         = 7
        docu_missing               = 8
        db_access_error            = 9
        action_aborted_by_user     = 10
        export_failed              = 11
        OTHERS                     = 12.

    IF sy-subrc <> 0.
      rv_fail = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
