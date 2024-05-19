*&---------------------------------------------------------------------*
*& Report ZTMP_TR_COPY
*&---------------------------------------------------------------------*
REPORT ztmp_tr_copy.

TABLES: sscrfields, e070.

CONSTANTS: gc_tr_copy_description TYPE e07t-as4text VALUE 'Copy of <TR>: <EPIC number> <Description>' ##NO_TEXT.

INCLUDE: <icon>.
INCLUDE: ztmp_tr_copy_cls.

*---------------------------------------------------------------------*
*    Selektionsbild
*---------------------------------------------------------------------*
SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_single RADIOBUTTON GROUP gr1 USER-COMMAND ucom1 DEFAULT 'X',
            p_multip RADIOBUTTON GROUP gr1.
SELECTION-SCREEN SKIP.
PARAMETERS: p_trkorr TYPE e070-trkorr    MODIF ID id1.
SELECT-OPTIONS: s_trkorr FOR e070-trkorr MODIF ID id2.
SELECTION-SCREEN: END OF BLOCK b1,
BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_astext LIKE e07t-as4text   MODIF ID id2 DEFAULT gc_tr_copy_description.
SELECTION-SCREEN SKIP.
PARAMETERS: p_cltsrc LIKE e070c-client   MODIF ID id3 DEFAULT sy-mandt,
            p_tarsys LIKE e070-tarsystem MODIF ID id3 DEFAULT 'FS3' MATCHCODE OBJECT tce_consolidation_targets.
SELECTION-SCREEN END OF BLOCK b2.

*---------------------------------------------------------------------*
*   Event  AT SELECTION-SCREEN
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      IF p_trkorr IS NOT INITIAL.
        SET PARAMETER ID 'KOR' FIELD p_trkorr.
        SUBMIT rddprott AND RETURN
          WITH pv_korr = p_trkorr.
      ENDIF.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

*---------------------------------------------------------------------*
*   Event  AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  IF p_single = abap_true.
    sscrfields-functxt_01 =
      VALUE smp_dyntxt(
        icon_id   = icon_display_text
        icon_text = 'Transport logs'(018)
      ).
  ELSE.
    CLEAR: sscrfields-functxt_01.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-group1 = 'ID1'.
      CASE abap_true.
        WHEN p_single.
          screen-active = '1'.
        WHEN p_multip.
          screen-active = '0'.
          CLEAR: p_trkorr.
      ENDCASE.
    ENDIF.
    IF screen-group1 = 'ID2'.
      CASE abap_true.
        WHEN p_single.
          screen-active = '0'.
          CLEAR: s_trkorr.
        WHEN p_multip.
          screen-active = '1'.
      ENDCASE.
    ENDIF.
    IF screen-group1 = 'ID3'.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

*---------------------------------------------------------------------*
*   Event  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_trkorr IS INITIAL AND s_trkorr IS INITIAL.
    MESSAGE TEXT-012 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM execute.

*---------------------------------------------------------------------*
*   Form  execute
*---------------------------------------------------------------------*
FORM execute.
  DATA: lt_parent  TYPE TABLE OF e070,
        lv_tr_copy TYPE trkorr,
        lv_error.

  ULINE.
  PERFORM select_requests_for_processing TABLES lt_parent.

  IF lt_parent IS NOT INITIAL.
    PERFORM create_tr_copy CHANGING lv_tr_copy.

    IF lv_tr_copy IS NOT INITIAL.
      LOOP AT lt_parent INTO DATA(ls_parent).
        PERFORM copy_parent_objects USING lv_tr_copy
                                          ls_parent
                                 CHANGING lv_error.
      ENDLOOP.

      IF lv_error IS INITIAL.
        PERFORM release_tr_copy USING lv_tr_copy.
      ENDIF.
    ENDIF.
  ENDIF.

  ULINE.
ENDFORM.

*---------------------------------------------------------------------*
*   Form  select_requests_for_processing
*---------------------------------------------------------------------*
FORM select_requests_for_processing TABLES ct_requests STRUCTURE e070.
  DATA: lrt_trkorr TYPE RANGE OF trkorr.

  CASE abap_true.
    WHEN p_single.
      IF p_trkorr IS NOT INITIAL.
        lrt_trkorr = VALUE #( ( sign = 'I' option = 'EQ' low = p_trkorr ) ).
      ENDIF.
    WHEN p_multip.
      lrt_trkorr = s_trkorr[].
  ENDCASE.

  IF lrt_trkorr IS INITIAL.
    MESSAGE 'Select TRs to copy'(012) TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    SELECT * FROM e070
     WHERE trkorr     IN @lrt_trkorr
       AND trfunction IN ('K','W')
       AND trstatus   EQ 'D'
      INTO TABLE @ct_requests.
  ENDIF.

  IF ct_requests[] IS INITIAL.
    WRITE: / '|', condense( TEXT-010 ), AT sy-linsz '|'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*   Form  get_tr_copy_text
*---------------------------------------------------------------------*
FORM get_tr_copy_text CHANGING cv_text TYPE e07t-as4text.
  DATA: lv_text TYPE string.

  CASE abap_true.
    WHEN p_single.
      SELECT SINGLE as4text FROM e07t
       WHERE trkorr = @p_trkorr
        INTO @DATA(lv_as4text).
      IF sy-subrc = 0.
        lv_text = |{ TEXT-016 } { p_trkorr }: { lv_as4text }|.
        IF strlen( lv_text ) > 60.
          lv_text = lv_text(57) && '...'.
        ENDIF.
        cv_text = lv_text.
      ENDIF.
    WHEN p_multip.
      IF p_astext = gc_tr_copy_description.
        MESSAGE 'Set valid TR description'(011) TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        cv_text = p_astext.
      ENDIF.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*   Form  create_tr_copy
*---------------------------------------------------------------------*
FORM create_tr_copy CHANGING cv_tr_copy.
  DATA: lv_text TYPE e07t-as4text.

  PERFORM get_tr_copy_text CHANGING lv_text.

  IF lv_text IS NOT INITIAL.
    cv_tr_copy = lcl_tr_processor=>create(
                   EXPORTING
                     iv_astext = lv_text
                     iv_trfunc = 'T'
                     iv_cltsrc = p_cltsrc
                     iv_tarsys = p_tarsys ).
  ENDIF.

  IF cv_tr_copy IS INITIAL.
    WRITE: / '|', condense( TEXT-003 ), AT sy-linsz '|'.
  ELSE.
    WRITE: / '|', condense( TEXT-004 ), (*) cv_tr_copy, condense( TEXT-005 ), AT sy-linsz '|'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*   Form  release_tr_copy
*---------------------------------------------------------------------*
FORM release_tr_copy USING iv_tr_copy.
  DATA(lv_fail) = lcl_tr_processor=>release( iv_trkorr = iv_tr_copy ).

  IF lv_fail = abap_true.
    WRITE: / '|', condense( TEXT-013 ), (*) iv_tr_copy, AT sy-linsz '|'.
  ELSE.
    WRITE: / '|', condense( TEXT-004 ), (*) iv_tr_copy, condense( TEXT-015 ), AT sy-linsz '|'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*   Form  copy_parent_objects
*---------------------------------------------------------------------*
FORM copy_parent_objects USING iv_tr_copy TYPE trkorr
                               is_parent  TYPE e070
                      CHANGING cv_error   TYPE abap_bool.

  DATA(lv_fail) = lcl_tr_processor=>copy_objects( iv_trkorr_fr = is_parent-trkorr
                                                  iv_trkorr_to = iv_tr_copy ).
  IF lv_fail = abap_true.
    cv_error = abap_true.
    WRITE: / '|', condense( TEXT-006 ),(*) is_parent-trkorr, condense( TEXT-007 ),(*) iv_tr_copy, AT sy-linsz '|'.
  ELSE.
    WRITE: / '|', condense( TEXT-006 ),(*) is_parent-trkorr, condense( TEXT-009 ),(*) iv_tr_copy, AT sy-linsz '|'.
  ENDIF.

  SELECT * FROM e070
   WHERE strkorr = @is_parent-trkorr
    INTO TABLE @DATA(lt_child).

  IF sy-subrc = 0.
    LOOP AT lt_child INTO DATA(ls_child).
      PERFORM copy_child_objects USING iv_tr_copy
                                       ls_child
                              CHANGING cv_error.
      SELECT * FROM e070
       WHERE strkorr = @ls_child-trkorr
       APPENDING TABLE @lt_child.
    ENDLOOP.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*   Form  copy_child_objects
*---------------------------------------------------------------------*
FORM copy_child_objects USING iv_tr_copy TYPE trkorr
                              is_child   TYPE e070
                     CHANGING cv_error   TYPE abap_bool.

  DATA(lv_fail) = lcl_tr_processor=>copy_objects( iv_trkorr_fr = is_child-trkorr
                                                  iv_trkorr_to = iv_tr_copy ).

  DATA(lv_is_task) = xsdbool( is_child-trfunction = 'S' OR is_child-trfunction = 'R' OR
                              is_child-trfunction = 'X' OR is_child-trfunction = 'Q' ).
  IF lv_fail = abap_true.
    cv_error = abap_true.
    IF lv_is_task = abap_true.
      WRITE: / '|', condense( TEXT-017 ), (*) is_child-trkorr, condense( TEXT-007 ), (*) iv_tr_copy, AT sy-linsz '|'.
    ELSE.
      WRITE: / '|', condense( TEXT-008 ), (*) is_child-trkorr, condense( TEXT-007 ), (*) iv_tr_copy, AT sy-linsz '|'.
    ENDIF.
  ELSE.
    IF lv_is_task = abap_true.
      WRITE: / '|', condense( TEXT-017 ), (*) is_child-trkorr, condense( TEXT-009 ), (*) iv_tr_copy, AT sy-linsz '|'.
    ELSE.
      WRITE: / '|', condense( TEXT-008 ), (*) is_child-trkorr, condense( TEXT-009 ), (*) iv_tr_copy, AT sy-linsz '|'.
    ENDIF.
  ENDIF.
ENDFORM.
