*&------------------------------------------------------------------------*
* Program ID    : ZRTR_TR_PRICING
* Creation date : 23-Dec-2019
* Author        : Nikolay Kozlov
*--------------------------------------------------------------------------
*   ########## ## ###
*
*   ######### ######### ###### ### ######### # ######### ########## ## ###
* (############ ###############), # ##### ####### ######## ##### #######.
*
* ITCR-30050 SAP ########## ### ############ ####### ###
*
* History of changes
*--------------------------------------------------------------------------
*  Date      | User-ID   |  Description
*--------------------------------------------------------------------------
* 23.12.2019   Kozlov     Initial version
* 27.05.2020   Kozlov     Add XML-form
* 17.08.2020   Kurgan     ITCR-33795 SAP ## ###### ### ######## xml # ### ########### ##############
*--------------------------------------------------------------------------

REPORT zrtr_tr_pricing.

INCLUDE zrtr_tr_pricing_top.

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: so_bukrs FOR bkpf-bukrs OBLIGATORY DEFAULT '1320'.
SELECT-OPTIONS: so_lifnr FOR bseg-lifnr.
SELECT-OPTIONS: so_kunnr FOR bseg-kunnr.
PARAMETERS: p_gjahr TYPE bkpf-gjahr OBLIGATORY DEFAULT sy-datum(4).
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-004.
PARAMETERS:
  p_zcode TYPE zalmfi_realtytaxes_scr-zcode,
  p_zoked TYPE char10.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_lname TYPE char20 LOWER CASE,
  p_fname TYPE char20 LOWER CASE,
  p_mname TYPE char20 LOWER CASE,
  p_phone TYPE char20.
SELECTION-SCREEN SKIP.
PARAMETERS:
  p_xmldir TYPE eseftfront.
SELECTION-SCREEN SKIP 1.
PARAMETERS:
  p_corrct AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b2.

INITIALIZATION.
  PERFORM initialize.

AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_xmldir.
  gv_xmldir = p_xmldir.
  PERFORM file_dialog CHANGING gv_xmldir.
  p_xmldir = gv_xmldir.

START-OF-SELECTION.

  PERFORM get_main_data CHANGING gt_data.
  PERFORM get_more_data CHANGING gt_data.

  CALL SCREEN 100.

  INCLUDE zrtr_tr_pricing_f01.
  INCLUDE zrtr_tr_pricing_alv.
  INCLUDE zrtr_tr_pricing_scr.
