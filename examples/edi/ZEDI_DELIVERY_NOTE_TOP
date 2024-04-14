*&---------------------------------------------------------------------*
*& Include ZEDI_DELIVERY_NOTE_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS: abap, icon.

CONSTANTS:
  gc_bsart_ub               TYPE ekko-bsart VALUE 'UB',
  gc_bwart_641              TYPE mseg-bwart VALUE '641',
  gc_waers_byr              TYPE waers      VALUE 'BYN',
  gc_lang                   TYPE adrc-langu VALUE 'R',

  gc_tn_type                TYPE zedi_delivery_note_type VALUE '270',
  gc_ttn_type               TYPE zedi_delivery_note_type VALUE '700',

  gc_id                     TYPE tdid     VALUE 'VERM',
  gc_object                 TYPE tdobject VALUE 'CHARGE',

  gc_id_material            TYPE tdid     VALUE 'GRUN',
  gc_object_material        TYPE tdobject VALUE 'MATERIAL',

  gc_objecttable            TYPE bapi1003_key-objecttable VALUE 'MCHA',
  gc_classnum               TYPE bapi1003_key-classnum    VALUE 'H132_MM_BATCH',
  gc_classtype              TYPE bapi1003_key-classtype   VALUE '022',

  gc_charact_country_origin TYPE bapi1003_alloc_values_char-charact    VALUE 'H132_MM_COUNTRY_ORIGIN',
  gc_charact_country_import TYPE bapi1003_alloc_values_char-charact    VALUE 'H132_MM_COUNTRY_IMPORT',
  gc_country_by             TYPE bapi1003_alloc_values_char-value_char VALUE 'BY',

  gc_alv_structure          TYPE tabname VALUE 'ZEDI_DELIVERY_NOTE_MASS_LIST'.

DATA: gs_030003      TYPE zdf_card_030003,
      gt_items       TYPE TABLE OF zdf_item_030003,
      gt_objectlinks TYPE /dfs/drad OCCURS 0 WITH HEADER LINE.

DATA: gs_ekko TYPE ekko,
      gt_ekpo TYPE TABLE OF ekpo,
      gs_mkpf TYPE mkpf,
      gt_mseg TYPE TABLE OF mseg,
      gt_likp TYPE likp OCCURS 0 WITH HEADER LINE.

* Таблица данных для создания карточек
DATA: gt_data TYPE TABLE OF zedi_delivery_note_mass_list,
      gs_data TYPE zedi_delivery_note_mass_list.

DATA: gr_alv               TYPE REF TO cl_gui_alv_grid,
      gr_container         TYPE REF TO cl_gui_custom_container,
      gt_fcatalog          TYPE lvc_t_fcat WITH HEADER LINE,
      gt_sorting           TYPE lvc_t_sort WITH HEADER LINE,
      gt_style             TYPE lvc_t_styl WITH HEADER LINE,
      gs_layout            TYPE lvc_s_layo,
      gt_toolbar_excluding TYPE ui_functions.

DATA: gv_log_handle TYPE balloghndl.

DATA: gt_msg TYPE bal_t_msg.

DATA: ok_code      LIKE sy-ucomm,
      ok_code_save LIKE sy-ucomm.
