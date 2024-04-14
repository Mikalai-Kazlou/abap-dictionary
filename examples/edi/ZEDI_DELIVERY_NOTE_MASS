*&-------------------------------------------------------------------------
* Program ID    : ZEDI_DELIVERY_NOTE_MASS
* Creation date : 30-Sep-2015
* Author        : Nikolay Kozlov
*--------------------------------------------------------------------------
*    ѕрограмма предназначена дл€ массового создани€ карточек
* документов "Ёлектронна€ накладна€".
*
* History of changes
*--------------------------------------------------------------------------
* Date       | User-ID   | Description
*--------------------------------------------------------------------------
* 30.09.2015   Kozlov      Initial version
*--------------------------------------------------------------------------

REPORT zedi_delivery_note_mass.

TABLES: likp, zdf_card_030003.

INCLUDE zedi_delivery_note_top.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
  so_vbeln FOR likp-vbeln MEMORY ID vl MATCHCODE OBJECT vmvl OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS:
  rb_tn  RADIOBUTTON GROUP rad,
  rb_ttn RADIOBUTTON GROUP rad DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.

  REFRESH: gt_data, gt_likp.

  SELECT vbeln anzpk btgew gewei FROM likp
    INTO CORRESPONDING FIELDS OF TABLE gt_likp
    WHERE vbeln IN so_vbeln.

  LOOP AT gt_likp.
    CLEAR: gs_data.

    gs_data-vbeln     = gt_likp-vbeln.
    gs_data-quan_unit = gt_likp-anzpk.
* --> 21.06.2018 Kozlov (ITCR-20859)
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = gt_likp-btgew
        unit_in              = gt_likp-gewei
        unit_out             = 'TO'
      IMPORTING
        output               = gs_data-gross_weight_val
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10.
    IF sy-subrc NE 0.
      CLEAR gs_data-gross_weight_val.
    ENDIF.
* <-- 21.06.2018 Kozlov (ITCR-20859)

    APPEND gs_data TO gt_data.
  ENDLOOP.

  CALL SCREEN 100.

  INCLUDE zedi_delivery_note_data.
  INCLUDE zedi_delivery_note_dfs.
  INCLUDE zvlc_bal_log.

  INCLUDE zedi_delivery_note_mass_0100.
  INCLUDE zedi_delivery_note_mass_alv.
  INCLUDE zedi_delivery_note_mass_forms.
