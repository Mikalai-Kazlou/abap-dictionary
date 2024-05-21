*&------------------------------------------------------------------------*
* Program ID    : ZVLC_REPORT
* Creation date : 01-Jah-2020
* Author        : Nikolay Kozlov
*--------------------------------------------------------------------------
*   <Наименование>
*
*   <Описание>
*
* ITCR-xxxx
*
* History of changes
*--------------------------------------------------------------------------
*  Date      | User-ID   |  Description
*--------------------------------------------------------------------------
* 01.01.2020   Kozlov     Initial version
*--------------------------------------------------------------------------

REPORT zvlc_report.

INCLUDE zvlc_report_top.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: so_bukrs FOR bkpf-bukrs OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM get_main_data CHANGING gt_data.

  CALL SCREEN 100.

  INCLUDE zvlc_report_f01.
  INCLUDE zvlc_report_alv.
  INCLUDE zvlc_report_scr.
