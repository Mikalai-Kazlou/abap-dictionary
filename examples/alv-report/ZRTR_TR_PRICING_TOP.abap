*&---------------------------------------------------------------------*
*&  Include           ZRTR_TR_PRICING_TOP
*&---------------------------------------------------------------------*

TABLES: bkpf, bseg, sscrfields.

TYPE-POOLS: zoxml.

TYPES: BEGIN OF ts_data.
    INCLUDE TYPE zrtr_tr_pricing_alv.
TYPES: END   OF ts_data.
TYPES: tt_data TYPE TABLE OF ts_data.

TYPES: BEGIN OF ts_data_unit.
TYPES: matnr         TYPE zrtr_tr_pricing_alv-matnr,
       sgtxt         TYPE zrtr_tr_pricing_alv-sgtxt,
       stawn         TYPE zrtr_tr_pricing_alv-stawn,
       inco1         TYPE zrtr_tr_pricing_alv-inco1,
       wrbtr         TYPE zrtr_tr_pricing_alv-wrbtr,
       wrbtr_str     TYPE string,
       waers         TYPE zrtr_tr_pricing_alv-waers,
       meins         TYPE zrtr_tr_pricing_alv-meins,
       meins_str     TYPE string,
       vatdate       TYPE zrtr_tr_pricing_alv-vatdate,
       vatdate_str   TYPE string,
       waers_ext_str TYPE string,
       waers_loc_str TYPE string.
TYPES: END   OF ts_data_unit.
TYPES: tt_data_unit TYPE TABLE OF ts_data_unit.

CLASS lcl_alv_grid DEFINITION DEFERRED.

CONSTANTS: gc_trade_type_01(20) TYPE c VALUE '#####',
           gc_trade_type_02(20) TYPE c VALUE '######(######)',
           gc_trade_type_03(20) TYPE c VALUE '############# #####'.

CONSTANTS: gc_purchase_target_01(30) TYPE c VALUE '######## ###########',
           gc_purchase_target_02(30) TYPE c VALUE '###########',
           gc_purchase_target_03(30) TYPE c VALUE '############# # ############'.

CONSTANTS: gc_print_form_doc TYPE zoxml_template_id VALUE 'ZTMPL_DOCUMENTATION',
           gc_print_form_rat TYPE zoxml_template_id VALUE 'ZTMPL_RATIONALE',
           gc_xml_form_doc   TYPE zedi_msg_type     VALUE 'ZFI_DOCUM_ECONOM_VALID_COST',
           gc_xml_form_rat   TYPE zedi_msg_type     VALUE 'ZFI_ECONOMIC_SUBSTANTION_APPIL'.

CONSTANTS: gc_dec_zero TYPE wrbtr VALUE '0.0'.

DATA: gs_data TYPE ts_data,
      gt_data TYPE tt_data.

DATA: gr_docking_empty TYPE REF TO cl_gui_docking_container,
      gr_ext_docking   TYPE REF TO cl_gui_container,
      gr_grid          TYPE REF TO lcl_alv_grid,
      gt_fieldcat      TYPE lvc_t_fcat,
      gt_sorting       TYPE lvc_t_sort,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant,
      gt_excluding     TYPE ui_functions.

DATA: gt_merge_table TYPE zoxml_merge_table,
      gs_merge_item  TYPE zoxml_merge_item.

DATA: gt_data_mat01 TYPE tt_data,
      gt_data_mat02 TYPE tt_data,
      gt_data_mat03 TYPE tt_data.

DATA: gt_data_unit01 TYPE tt_data_unit,
      gt_data_unit02 TYPE tt_data_unit,
      gt_data_unit03 TYPE tt_data_unit.

DATA: gs_header_data TYPE ztd_pribyl,
      gv_xmldir      TYPE string.

DATA: BEGIN OF gs_str.
DATA: str01 TYPE string,
      str02 TYPE string,
      str03 TYPE string,
      str04 TYPE string,
      str05 TYPE string,
      str06 TYPE string,
      str07 TYPE string,
      str08 TYPE string,
      str09 TYPE string,
      str10 TYPE string,
      str11 TYPE string,
      str12 TYPE string,
      str13 TYPE string,
      str14 TYPE string,
      str15 TYPE string,
      str16 TYPE string,
      str17 TYPE string,
      str18 TYPE string,
      str19 TYPE string,
      str20 TYPE string,
      str21 TYPE string,
      str22 TYPE string,
      str23 TYPE string,
      str24 TYPE string,
      str25 TYPE string,
      str26 TYPE string,
      str27 TYPE string,
      str28 TYPE string,
      str29 TYPE string,
      str30 TYPE string,
      str31 TYPE string,
      str32 TYPE string,
      str33 TYPE string,
      str34 TYPE string,
      str35 TYPE string,
      str36 TYPE string,
      str37 TYPE string,
      str38 TYPE string,
      str39 TYPE string,
      str40 TYPE string,
      str41 TYPE string,
      str42 TYPE string,
      str43 TYPE string,
      str44 TYPE string,
      str45 TYPE string,
      str46 TYPE string,
      str47 TYPE string,
      str48 TYPE string,
      str49 TYPE string,
      str50 TYPE string.
DATA: END   OF gs_str.

DATA: ok_code LIKE sy-ucomm,
      save_ok LIKE sy-ucomm.


*--> start Kurgan 15.07.2020 ITCR-33795 SAP ## ###### ### ######## xml # ### ########### ##############
DATA: gs_head TYPE ztd_ecjustifapprice_head,
      gs_body TYPE ztd_ecjustifapprice.
*<-- end Kurgan 15.07.2020 ITCR-33795 SAP ## ###### ### ######## xml # ### ########### ##############
