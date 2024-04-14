*&-------------------------------------------------------------------------
* Program ID    : ZEDI_DELIVERY_NOTE
* Creation date : 04-Mar-2015
* Author        : Nikolay Kozlov
*--------------------------------------------------------------------------
*    Программа предназначена для создания карточек
* документов "Электронная накладная".
*
* History of changes
*--------------------------------------------------------------------------
* Date       | User-ID   | Description
*--------------------------------------------------------------------------
* 04.03.2015   Kozlov      Initial version
*--------------------------------------------------------------------------

REPORT zedi_delivery_note.

INCLUDE zedi_delivery_note_top.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:
  p_vbeln TYPE likp-vbeln MEMORY ID vl MATCHCODE OBJECT vmvl OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:
  rb_tn  RADIOBUTTON GROUP rad,
  rb_ttn RADIOBUTTON GROUP rad DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.

  REFRESH:
    gt_ekpo, gt_objectlinks,
    gt_mseg, gt_items,
    gt_msg.

  CLEAR:
    gs_ekko, gs_030003,
    gs_mkpf.

* Получим ММ-данные
  PERFORM get_mm_data TABLES gt_mseg gt_ekpo
                       USING p_vbeln
                    CHANGING gs_mkpf gs_ekko.

* Первичные проверки данных
  PERFORM check_mm_data TABLES gt_mseg gt_ekpo gt_msg
                         USING p_vbeln gs_mkpf gs_ekko abap_true
                      CHANGING gs_data.

* Получим данные для создания карточки
  PERFORM get_dfs_data TABLES gt_items gt_objectlinks gt_mseg gt_ekpo
                        USING p_vbeln gs_mkpf gs_ekko
                     CHANGING gs_030003.

* Проверим данные перед созданием карточки
  PERFORM check_dfs_data TABLES gt_items gt_msg
                          USING gs_030003.

* Создадим карточку
  IF LINES( gt_msg ) = 0.
    PERFORM create_card TABLES gt_items gt_objectlinks gt_msg
                         USING gs_030003 abap_true.
  ELSE.
*   Запишем лог
    PERFORM write_log TABLES gt_msg
                       USING 'ZVLC_EDI'
                             'ZEDI_DELIVERY_NOTE'.
*   Покажем лог
    PERFORM show_log TABLES gt_msg.
  ENDIF.

  INCLUDE zedi_delivery_note_data.
  INCLUDE zedi_delivery_note_dfs.
  INCLUDE zvlc_bal_log.
