*&---------------------------------------------------------------------*
*&  Include           ZRTR_TR_PRICING_F01
*&---------------------------------------------------------------------*

DEFINE set_xml_value.

  IF &2 IS NOT INITIAL.
    CLEAR gs_merge_item.

    gs_merge_item-name = &1.
    GET REFERENCE OF &2 INTO gs_merge_item-value_ptr.

    APPEND gs_merge_item TO gt_merge_table.
  ENDIF.

END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  GET_MAIN_DATA
*&---------------------------------------------------------------------*
FORM get_main_data CHANGING ct_data TYPE tt_data.

  DATA: lt_bsik TYPE TABLE OF bsik,
        lt_bsak TYPE TABLE OF bsak,
        lt_bsid TYPE TABLE OF bsid,
        lt_bsad TYPE TABLE OF bsad.

  DATA: lt_bseg TYPE TABLE OF bseg,
        ls_bseg TYPE bseg,
        lt_bkpf TYPE TABLE OF bkpf,
        ls_bkpf TYPE bkpf.

  DATA: ls_data     TYPE ts_data,
        ls_rbkp     TYPE rbkp,
        lt_bseg_doc TYPE TABLE OF bseg,
        ls_bseg_doc TYPE bseg,
        lv_tabix    TYPE sy-tabix.

* ############ #######
  IF so_lifnr IS NOT INITIAL.
    SELECT bukrs lifnr gjahr belnr buzei
      FROM bsik APPENDING CORRESPONDING FIELDS OF TABLE lt_bseg
      WHERE bukrs IN so_bukrs
        AND lifnr IN so_lifnr
        AND gjahr EQ p_gjahr
        AND shkzg EQ 'H'.

    SELECT bukrs lifnr gjahr belnr buzei
      FROM bsak APPENDING CORRESPONDING FIELDS OF TABLE lt_bseg
      WHERE bukrs IN so_bukrs
        AND lifnr IN so_lifnr
        AND gjahr EQ p_gjahr
        AND shkzg EQ 'H'.
  ENDIF.

* ########### #######
  IF so_kunnr IS NOT INITIAL.
    SELECT bukrs kunnr gjahr belnr buzei
      FROM bsid APPENDING CORRESPONDING FIELDS OF TABLE lt_bseg
      WHERE bukrs IN so_bukrs
        AND kunnr IN so_kunnr
        AND gjahr EQ p_gjahr
        AND shkzg EQ 'S'.

    SELECT bukrs kunnr gjahr belnr buzei
      FROM bsad APPENDING CORRESPONDING FIELDS OF TABLE lt_bseg
      WHERE bukrs IN so_bukrs
        AND kunnr IN so_kunnr
        AND gjahr EQ p_gjahr
        AND shkzg EQ 'S'.
  ENDIF.

  IF lines( lt_bseg ) > 0.
    SELECT * FROM bseg INTO TABLE lt_bseg
      FOR ALL ENTRIES IN lt_bseg
      WHERE bukrs = lt_bseg-bukrs
        AND belnr = lt_bseg-belnr
        AND gjahr = lt_bseg-gjahr
        AND buzei = lt_bseg-buzei.
    IF sy-subrc = 0.
      "######## #######-######
      DELETE lt_bseg WHERE xnegp NE ''.

      IF lines( lt_bseg ) > 0.
        SELECT * FROM bkpf INTO TABLE lt_bkpf
          FOR ALL ENTRIES IN lt_bseg
          WHERE bukrs = lt_bseg-bukrs
            AND belnr = lt_bseg-belnr
            AND gjahr = lt_bseg-gjahr.
        IF sy-subrc = 0.
          "######## "######" # ############## #########
          DELETE lt_bkpf WHERE blart = 'ZP'
                            OR blart = 'DZ'
                            OR blart = 'KZ'
                            OR blart = 'AB'
                            OR stblg NE ''.

          IF lines( lt_bkpf ) > 0.
            LOOP AT lt_bkpf INTO ls_bkpf.
              lv_tabix = sy-tabix.

              IF ls_bkpf-awtyp = 'RMRP' AND ls_bkpf-awkey IS NOT INITIAL.
                SELECT SINGLE * FROM rbkp INTO ls_rbkp
                  WHERE belnr = ls_bkpf-awkey(10)
                    AND gjahr = ls_bkpf-awkey+10.
                IF sy-subrc = 0.
                  IF ls_rbkp-stblg IS NOT INITIAL.
                    DELETE lt_bkpf INDEX lv_tabix.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

          "######## ########### #########
          IF lines( lt_bkpf ) > 0.
            LOOP AT lt_bkpf INTO ls_bkpf.
              lv_tabix = sy-tabix.

              REFRESH lt_bseg_doc.
              SELECT * FROM bseg INTO TABLE lt_bseg_doc
                WHERE bukrs = ls_bkpf-bukrs
                  AND belnr = ls_bkpf-belnr
                  AND gjahr = ls_bkpf-gjahr.
              IF sy-subrc = 0.
                LOOP AT lt_bseg_doc INTO ls_bseg_doc.
                  IF ls_bseg_doc-kostl = '0001100002'. "####### #######
                    DELETE lt_bkpf INDEX lv_tabix.
                    EXIT.
                  ENDIF.
                ENDLOOP.
              ENDIF.

            ENDLOOP.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* ####### ################# ####### ##########
  LOOP AT lt_bkpf INTO ls_bkpf.
    REFRESH lt_bseg_doc.
    SELECT * FROM bseg INTO TABLE lt_bseg_doc
      WHERE bukrs = ls_bkpf-bukrs
        AND belnr = ls_bkpf-belnr
        AND gjahr = ls_bkpf-gjahr
        AND lifnr = ''
        AND kunnr = ''.
    IF sy-subrc = 0.
      "###### ############/########### ####### #########
      READ TABLE lt_bseg INTO ls_bseg WITH KEY bukrs = ls_bkpf-bukrs
                                               belnr = ls_bkpf-belnr
                                               gjahr = ls_bkpf-gjahr.
      IF sy-subrc <> 0.
        CLEAR: ls_bseg.
      ENDIF.

      "######### ######## #######
      LOOP AT lt_bseg_doc INTO ls_bseg_doc.
        MOVE-CORRESPONDING: ls_bseg_doc TO ls_data.

        ls_data-lifnr = ls_bseg-lifnr.
        ls_data-kunnr = ls_bseg-kunnr.

        IF ls_data-vertn IS INITIAL.
          ls_data-vertn = ls_bseg-vertn.
        ENDIF.

        ls_data-budat   = ls_bkpf-budat.
        ls_data-bldat   = ls_bkpf-bldat.
        ls_data-vatdate = ls_bkpf-vatdate.
        ls_data-waers   = ls_bkpf-waers.
        ls_data-awtyp   = ls_bkpf-awtyp.
        ls_data-awkey   = ls_bkpf-awkey.

        APPEND ls_data TO ct_data.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

* ######### ######## #######
  SORT ct_data BY lifnr kunnr vertn budat.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_MORE_DATA
*&---------------------------------------------------------------------*
FORM get_more_data CHANGING ct_data TYPE tt_data.

  DATA: lv_str TYPE string.

  LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<fs_data>).

    " ######
    SELECT SINGLE waers FROM t001 INTO <fs_data>-laers
      WHERE bukrs = <fs_data>-bukrs.

    " ###### ########
    PERFORM read_contract_data CHANGING <fs_data>.

    " ##### ####### #######
    IF <fs_data>-zterm IS NOT INITIAL.
      SELECT SINGLE text1 FROM t052u INTO <fs_data>-zterm_txt
        WHERE spras = sy-langu
          AND zterm = <fs_data>-zterm.
    ENDIF.

    " ###### ########
    CALL FUNCTION 'Z030_GET_LIFNR_KUNNR_DESCR'
      EXPORTING
        i_lifnr         = <fs_data>-lifnr
        i_kunnr         = <fs_data>-kunnr
      IMPORTING
        e_description   = lv_str
        e_country       = <fs_data>-country
        e_sort1         = <fs_data>-bp_unp
      EXCEPTIONS
        wrong_parameter = 1
        data_not_found  = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      <fs_data>-bp_name = lv_str.
    ENDIF.

    " #### #####
    IF <fs_data>-menge IS NOT INITIAL.
      <fs_data>-netpr = <fs_data>-wrbtr / <fs_data>-menge.
    ENDIF.

    " #### ### ########### #### ############
    PERFORM get_hkont CHANGING <fs_data>.

    " ###### #########
    PERFORM read_material_data CHANGING <fs_data>.

    " ###### ######
    PERFORM read_batch_data CHANGING <fs_data>.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_hkont
*&---------------------------------------------------------------------*
FORM get_hkont CHANGING cs_data TYPE ts_data.

  DATA: lv_lfgja TYPE ekbe-lfgja,
        lv_lfbnr TYPE ekbe-lfbnr,
        lv_lfpos TYPE ekbe-lfpos.

  SELECT SINGLE lfgja lfbnr lfpos FROM ekbe INTO ( lv_lfgja, lv_lfbnr, lv_lfpos )
    WHERE ebeln = cs_data-ebeln
      AND ebelp = cs_data-ebelp
      AND gjahr = cs_data-awkey+10
      AND belnr = cs_data-awkey(10).
  IF sy-subrc = 0.
    SELECT SINGLE hkont FROM acctit INTO cs_data-hkont
      WHERE awtyp = 'MKPF'
        AND awref = lv_lfbnr
        AND aworg = lv_lfgja
        AND shkzg = 'S'
        AND ebeln = cs_data-ebeln
        AND ebelp = cs_data-ebelp.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  read_contract_data
*&---------------------------------------------------------------------*
FORM read_contract_data CHANGING cs_data TYPE ts_data.

  DATA: ls_keyfields TYPE /dfs/str_keyfields,
        ls_card_data TYPE zdf_card_999011.

* ####### ######## ######### ########
  CALL FUNCTION 'Z030_GET_DFS_KEYFIELD_BY_REGNR'
    EXPORTING
      i_regid         = cs_data-vertn
    IMPORTING
      es_keyfields    = ls_keyfields
    EXCEPTIONS
      wrong_parameter = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
    CALL FUNCTION '/DFS/GET_HIER_TOP_CARD'
      EXPORTING
        is_card_key  = ls_keyfields
      IMPORTING
        top_card_key = ls_keyfields.

*   ######## ######### ########
    cs_data-vertn = ls_keyfields-doknr+12.

    CALL FUNCTION 'ZDF_API_GET_CARD_INFO_999011'
      EXPORTING
        keyfield             = ls_keyfields
      IMPORTING
        data_card            = ls_card_data
      EXCEPTIONS
        card_not_found       = 1
        error_read_card_data = 2
        break_enhancement    = 3
        critical_error       = 4
        application_error    = 5
        OTHERS               = 6.
    IF sy-subrc = 0.
      cs_data-otn_type   = ls_card_data-otn_type.
      cs_data-cn_type    = ls_card_data-cn_type.
      cs_data-cn_subtype = ls_card_data-cn_subtype.
      cs_data-cn_number  = ls_card_data-cn_number.
      cs_data-cn_date    = ls_card_data-cn_date.
      cs_data-zterm      = ls_card_data-zterm.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  read_batch_data
*&---------------------------------------------------------------------*
FORM read_batch_data CHANGING cs_data TYPE ts_data.

  DATA l_objecttable      TYPE bapi1003_key-objecttable.
  DATA l_classnum         TYPE bapi1003_key-classnum.
  DATA l_classtype        TYPE bapi1003_key-classtype.
  DATA l_language         TYPE bapifieldscacl-bapilangua.
  DATA lt_allocvaluesnum  TYPE STANDARD TABLE OF bapi1003_alloc_values_num.
  DATA lt_allocvalueschar TYPE STANDARD TABLE OF bapi1003_alloc_values_char.
  DATA lw_allocvalueschar TYPE bapi1003_alloc_values_char.
  DATA lt_allocvaluescurr TYPE STANDARD TABLE OF bapi1003_alloc_values_curr.
  DATA l_objectkey        TYPE bapi1003_key-object.
  DATA lt_return          TYPE STANDARD TABLE OF bapiret2.

  l_objecttable = 'MCHA'.
  l_classnum    = 'H132_MM_BATCH'.
  l_classtype   = '022'.
  l_language    = 'R'.

  IF cs_data-matnr IS NOT INITIAL AND
     cs_data-werks IS NOT INITIAL AND
     cs_data-bwtar IS NOT INITIAL.

    CONCATENATE cs_data-matnr
                cs_data-werks
                cs_data-bwtar INTO l_objectkey RESPECTING BLANKS.
  ELSE.
    EXIT.
  ENDIF.

  REFRESH: lt_allocvaluesnum,
           lt_allocvalueschar,
           lt_allocvaluescurr,
           lt_return.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey       = l_objectkey
      objecttable     = l_objecttable
      classnum        = l_classnum
      classtype       = l_classtype
      language        = l_language
    TABLES
      allocvaluesnum  = lt_allocvaluesnum
      allocvalueschar = lt_allocvalueschar
      allocvaluescurr = lt_allocvaluescurr
      return          = lt_return.

  IF lt_allocvalueschar[] IS NOT INITIAL.
    " ###### #############
    READ TABLE lt_allocvalueschar INTO lw_allocvalueschar
      WITH KEY charact = 'H132_MM_COUNTRY_ORIGIN'.
    IF sy-subrc EQ 0.
      cs_data-country_origin = lw_allocvalueschar-value_char.
    ELSE.
      " ###### #####
      READ TABLE lt_allocvalueschar INTO lw_allocvalueschar
        WITH KEY charact = 'H132_MM_COUNTRY_IMPORT'.
      IF sy-subrc EQ 0.
        cs_data-country_origin = lw_allocvalueschar-value_char.
      ENDIF.
    ENDIF.
  ELSE.
    cs_data-country_origin = 'BY'.
  ENDIF.

ENDFORM.                    "read_batch_data

*&---------------------------------------------------------------------*
*&      Form  read_material_data
*&---------------------------------------------------------------------*
FORM read_material_data CHANGING cs_data TYPE ts_data.

* ### ######
  PERFORM get_trade_type CHANGING cs_data.

* #### ############
  PERFORM get_purchase_target CHANGING cs_data.

* ### #####
  SELECT SINGLE stawn FROM marc INTO cs_data-stawn
    WHERE matnr = cs_data-matnr
      AND werks = cs_data-werks.

* #########
  IF cs_data-trade_type = gc_trade_type_01. "#####
    IF cs_data-country = 'RU'	    "######
      OR cs_data-country = 'KZ'	  "#########
      OR cs_data-country = 'AM'	  "#######
      OR cs_data-country = 'KG'.  "########
      cs_data-inco1 = 'DAP'.
    ELSE.
      cs_data-inco1 = 'DDU'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_trade_type
*&---------------------------------------------------------------------*
FORM get_trade_type CHANGING cs_data TYPE ts_data.

  DATA: ls_generaldata   TYPE bapimatdoa,
        ls_plantdata     TYPE bapimatdoc,
        ls_valuationdata TYPE bapimatdobew,
        ls_return        TYPE bapireturn.

  DATA: lv_bklas TYPE mbew-bklas.

* ### ######:
* 1) #####
* 2) ######(######)
* 3) ############# #####

  "#### ### ######## - ######
  IF cs_data-trade_type IS INITIAL.
    IF cs_data-cn_type = 10.
      cs_data-trade_type = gc_trade_type_02. "######(######)
    ENDIF.
  ENDIF.

  "#### ### ###### FI
  IF cs_data-trade_type IS INITIAL.
    IF cs_data-awtyp = 'BKPF'.
      cs_data-trade_type = gc_trade_type_02. "######(######)
    ENDIF.
  ENDIF.

  "########## ### ###### ## #### ######### # ###### ######
  IF cs_data-trade_type IS INITIAL.
    IF cs_data-matnr IS NOT INITIAL.
      CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
        EXPORTING
          material              = cs_data-matnr
          plant                 = cs_data-werks
          valuationarea         = cs_data-bwkey
          valuationtype         = cs_data-bwtar
        IMPORTING
          material_general_data = ls_generaldata
          return                = ls_return
          materialplantdata     = ls_plantdata
          materialvaluationdata = ls_valuationdata.

      CASE ls_generaldata-matl_type.
        WHEN 'HAWA'.
          cs_data-trade_type = gc_trade_type_01. "#####
        WHEN 'ZCOM'.
          cs_data-trade_type = gc_trade_type_01. "#####
        WHEN 'ZALX'.
          PERFORM get_bklas USING cs_data-matnr
                                  cs_data-bwkey
                                  cs_data-bwtar
                         CHANGING lv_bklas.

          IF lv_bklas = '6033' OR
             lv_bklas = '6047' OR
             lv_bklas = '6048' OR
             lv_bklas = '6133' OR
             lv_bklas = '6147' OR
             lv_bklas = '6148'.
            cs_data-trade_type = gc_trade_type_03. "############# #####
          ELSE.
            cs_data-trade_type = gc_trade_type_01. "#####
          ENDIF.
        WHEN 'NLAG'.
          PERFORM get_bklas USING cs_data-matnr
                                  cs_data-bwkey
                                  cs_data-bwtar
                         CHANGING lv_bklas.

          IF lv_bklas = '0004' OR
             lv_bklas = '0005' OR
             lv_bklas = '0006' OR
             lv_bklas = '0007' OR
             lv_bklas = '0145' OR
             lv_bklas = '6034'.
            cs_data-trade_type = gc_trade_type_03. "############# #####
          ELSE.
            cs_data-trade_type = gc_trade_type_02. "######(######)
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_purchase_target
*&---------------------------------------------------------------------*
FORM get_purchase_target CHANGING cs_data TYPE ts_data.

  DATA: lv_pt TYPE zvlc_purch_target_list.

* #### ############:
* 1) ######## ###########
* 2) ###########
* 3) ############# # ############

  IF cs_data-kunnr IS NOT INITIAL.
    cs_data-purch_target = gc_purchase_target_01. "######## ###########
    EXIT.
  ENDIF.

  CASE cs_data-trade_type.
    WHEN gc_trade_type_01. "######
      SELECT SINGLE purch_target FROM zrtr_pt_set INTO lv_pt
        WHERE bukrs = cs_data-bukrs
          AND hkont = cs_data-hkont.
      IF sy-subrc = 0.
        CASE lv_pt.
          WHEN 'E'. cs_data-purch_target = gc_purchase_target_01. "######## ###########
          WHEN 'S'. cs_data-purch_target = gc_purchase_target_02. "###########
          WHEN 'P'. cs_data-purch_target = gc_purchase_target_03. "############# # ############
        ENDCASE.
      ENDIF.
    WHEN gc_trade_type_02. "######(######)
      cs_data-purch_target = gc_purchase_target_01. "######## ###########
    WHEN gc_trade_type_03. "############# #####
      cs_data-purch_target = gc_purchase_target_03. "############# # ############
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_bklas
*&---------------------------------------------------------------------*
FORM get_bklas USING iv_matnr TYPE ts_data-matnr
                     iv_bwkey TYPE ts_data-bwkey
                     iv_bwtar TYPE ts_data-bwtar
            CHANGING cv_bklas TYPE mbew-bklas.

  SELECT SINGLE bklas FROM mbew INTO cv_bklas
    WHERE matnr = iv_matnr
      AND bwkey = iv_bwkey
      AND bwtar = iv_bwtar.

  IF sy-subrc NE 0.
    CLEAR: cv_bklas.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CREATE_FORM
*&---------------------------------------------------------------------*
FORM create_form USING iv_operation
                       iv_template_id TYPE zoxml_template_id
                       iv_msg_type TYPE zedi_msg_type.

  DATA: lt_data          TYPE tt_data,
        lt_data_group    TYPE tt_data,
        lv_set_number(2) TYPE n.

  lt_data[] = gt_data[].
  DELETE lt_data WHERE zflag = ''.

  LOOP AT lt_data INTO DATA(ls_data)
         GROUP BY ( lifnr   = ls_data-lifnr
                    kunnr   = ls_data-kunnr
                    cn_type = ls_data-cn_type
                    waers   = ls_data-waers
                    size = GROUP SIZE index = GROUP INDEX )
                  ASCENDING
                  REFERENCE INTO DATA(lr_group).

    REFRESH: gt_merge_table.
    lv_set_number = sy-tabix.

    REFRESH: lt_data_group.
    LOOP AT GROUP lr_group ASSIGNING FIELD-SYMBOL(<fs_data>).
      lt_data_group = VALUE #( BASE lt_data_group ( <fs_data> ) ).
    ENDLOOP.

    PERFORM get_merge_table USING lt_data_group.

    CASE iv_operation.
      WHEN 'PRINT'.
        PERFORM print_oxml USING iv_template_id
                                 gt_merge_table.
      WHEN 'XML'.
        PERFORM save_xml USING iv_msg_type
                               gt_merge_table
                               lt_data_group
                               lv_set_number.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  set_header_data
*&---------------------------------------------------------------------*
FORM set_header_data.

  DATA:
    l_name2      TYPE ad_name2,
    l_name3      TYPE ad_name3,
    l_post_code1 TYPE ad_pstcd1,
    l_city1      TYPE ad_city1,
    l_street     TYPE ad_street,
    l_house_num1 TYPE ad_hsnm1.

  CLEAR: gs_header_data.

* ####### ######## #########
  IF NOT p_corrct IS INITIAL.
    MOVE 'X' TO gs_header_data-corrct.
  ENDIF.

* ### ####
  MOVE p_zcode TO gs_header_data-zcode.

* ### ####
  MOVE p_zoked TO gs_header_data-zoked.

* ######## ####
  SELECT SINGLE zname FROM zalmfi_taximns INTO gs_header_data-zname
    WHERE zcode = p_zcode.

* ###
  SELECT SINGLE paval FROM t001z INTO gs_header_data-paval
    WHERE bukrs IN so_bukrs AND party = 'SAPR01'.

* ############ # ##### ###########
  SELECT SINGLE b~name2 b~name3 b~post_code1 b~city1 b~street b~house_num1
    INTO (l_name2, l_name3, l_post_code1, l_city1, l_street, l_house_num1)
     FROM t001 AS a INNER JOIN adrc AS b ON
       a~adrnr = b~addrnumber
     WHERE a~bukrs IN so_bukrs.

  IF sy-subrc = 0.
    CONCATENATE l_name2 l_name3 INTO gs_header_data-payer_name SEPARATED BY space.
    CONCATENATE l_post_code1 l_city1 l_street l_house_num1 INTO gs_header_data-addr_info
                SEPARATED BY space.

    REPLACE ALL OCCURRENCES OF '########## #' IN gs_header_data-payer_name WITH '###########'.
  ENDIF.

* ###
  CONCATENATE p_lname p_fname p_mname p_phone INTO gs_header_data-fio SEPARATED BY space.

* ########## ###
  MOVE p_gjahr TO gs_header_data-gjahr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_merge_table
*&---------------------------------------------------------------------*
FORM get_merge_table USING it_data_group TYPE tt_data.

  DATA: ls_data_group   TYPE ts_data,
        lt_data_collect TYPE tt_data,
        ls_data_collect TYPE ts_data,
        ls_data_unit    TYPE ts_data_unit.

  DATA: lv_str            TYPE string,
        lv_str_date(10)   TYPE c,
        lv_zterm          TYPE dzterm,
        lv_waers          TYPE waers,
        gc_dec_zero       TYPE wrbtr,
        gc_dec_zero_c(15) TYPE c,
        lv_datum          TYPE datum,
        lv_datum_c(10)    TYPE c,
        ls_t006a          TYPE t006a.

  REFRESH: gt_data_mat01,
           gt_data_mat02,
           gt_data_mat03.

  REFRESH: gt_data_unit01,
           gt_data_unit02,
           gt_data_unit03.

  CLEAR: gs_str.

* ###### #####
  PERFORM set_header_data.
  set_xml_value 'HD' gs_header_data.

*--> start Kurgan 15.07.2020 ITCR-33795 SAP ## ###### ### ######## xml # ### ########### ##############
  MOVE-CORRESPONDING gs_header_data TO gs_head.
*<-- end Kurgan 15.07.2020 ITCR-33795 SAP ## ###### ### ######## xml # ### ########### ##############

* ###### #########
  lt_data_collect[] = it_data_group[].
  SORT lt_data_collect BY cn_number cn_date.
  DELETE ADJACENT DUPLICATES FROM lt_data_collect COMPARING cn_number cn_date.

* (1.1) ##### ########
  CLEAR: lv_str.
  LOOP AT lt_data_collect INTO ls_data_collect.
    CONCATENATE lv_str ';' ls_data_collect-cn_number INTO lv_str SEPARATED BY space.
  ENDLOOP.
  IF lv_str IS NOT INITIAL.
    lv_str = lv_str+3.
  ENDIF.
  IF lines( lt_data_collect ) > 1.
    gs_str-str38 = lv_str.
  ELSE.
    gs_str-str01 = lv_str.
  ENDIF.

* (1.1) #### ########
  CLEAR: lv_str.
  LOOP AT lt_data_collect INTO ls_data_collect.
    WRITE ls_data_collect-cn_date TO lv_str_date DD/MM/YYYY.
    CONCATENATE lv_str ';' lv_str_date INTO lv_str SEPARATED BY space.
  ENDLOOP.
  IF lv_str IS NOT INITIAL.
    lv_str = lv_str+3.
  ENDIF.
  IF lines( lt_data_collect ) > 1.
    gs_str-str39 = lv_str.
  ELSE.
    gs_str-str02 = lv_str.
  ENDIF.

* (1.2) ###### ########## ######
  CLEAR: lv_str.
  IF lines( lt_data_collect ) > 1.
    gs_str-str40 = '##'.
  ELSE.
    gs_str-str40 = '###'.
  ENDIF.

* (1.3) ### ######
  LOOP AT it_data_group INTO ls_data_group WHERE trade_type = gc_trade_type_01 AND kunnr NE ''.
  ENDLOOP.
  IF sy-subrc = 0.
    gs_str-str03 = 'X'.
  ENDIF.
  LOOP AT it_data_group INTO ls_data_group WHERE trade_type = gc_trade_type_02 AND kunnr NE ''.
  ENDLOOP.
  IF sy-subrc = 0.
    gs_str-str04 = 'X'.
  ENDIF.
  LOOP AT it_data_group INTO ls_data_group WHERE trade_type = gc_trade_type_03 AND kunnr NE ''.
  ENDLOOP.
  IF sy-subrc = 0.
    gs_str-str05 = 'X'.
  ENDIF.

  LOOP AT it_data_group INTO ls_data_group WHERE trade_type = gc_trade_type_01 AND lifnr NE ''.
  ENDLOOP.
  IF sy-subrc = 0.
    gs_str-str07 = 'X'.
  ENDIF.
  LOOP AT it_data_group INTO ls_data_group WHERE trade_type = gc_trade_type_02 AND lifnr NE ''.
  ENDLOOP.
  IF sy-subrc = 0.
    gs_str-str08 = 'X'.
  ENDIF.
  LOOP AT it_data_group INTO ls_data_group WHERE trade_type = gc_trade_type_03 AND lifnr NE ''.
  ENDLOOP.
  IF sy-subrc = 0.
    gs_str-str09 = 'X'.
  ENDIF.

* (1.4) #### ############
  LOOP AT it_data_group INTO ls_data_group WHERE purch_target = gc_purchase_target_01.
  ENDLOOP.
  IF sy-subrc = 0.
    gs_str-str11 = 'X'.
  ENDIF.
  LOOP AT it_data_group INTO ls_data_group WHERE purch_target = gc_purchase_target_02.
  ENDLOOP.
  IF sy-subrc = 0.
    gs_str-str12 = 'X'.
  ENDIF.
  LOOP AT it_data_group INTO ls_data_group WHERE purch_target = gc_purchase_target_03.
  ENDLOOP.
  IF sy-subrc = 0.
    gs_str-str13 = 'X'.
  ENDIF.

* (1.6) ####### ############# ########
  READ TABLE it_data_group INTO ls_data_group INDEX 1.
  IF sy-subrc = 0.
    CLEAR: lv_zterm.
    SELECT SINGLE zterm FROM t052s INTO lv_zterm
      WHERE zterm = ls_data_group-zterm.

    IF lv_zterm IS NOT INITIAL OR ls_data_group-zterm = 'Z000'.
      gs_str-str14 = 'X'.
      gs_str-str17 = ls_data_group-zterm_txt.
    ELSE.
      gs_str-str15 = 'X'.
      gs_str-str18 = ls_data_group-zterm_txt.
    ENDIF.
  ENDIF.

* (2.1) ######## # ########### ####
  READ TABLE it_data_group INTO ls_data_group INDEX 1.
  IF sy-subrc = 0.
    gs_str-str20 = ls_data_group-bp_name.
    gs_str-str21 = ls_data_group-bp_unp.
  ENDIF.

* (2.4) ###### ########### ###########
  READ TABLE it_data_group INTO ls_data_group INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE landx50 FROM t005t INTO gs_str-str22
      WHERE spras = 'R'
        AND land1 = ls_data_group-country.
    IF sy-subrc <> 0.
      CLEAR: gs_str-str22.
    ENDIF.
  ENDIF.

*** ###### ----------------------------------------------------------------------
  gt_data_mat01[] = it_data_group[].
  DELETE gt_data_mat01 WHERE trade_type NE gc_trade_type_01.
  IF lines( gt_data_mat01 ) > 0.

    LOOP AT gt_data_mat01 INTO ls_data_group.
      MOVE-CORRESPONDING: ls_data_group TO ls_data_unit.
      COLLECT ls_data_unit INTO gt_data_unit01.
    ENDLOOP.

    LOOP AT gt_data_unit01 INTO ls_data_unit.
*     (6.1.3) ######### ######
      IF ls_data_unit-waers = 'BYN'.
        ls_data_unit-waers_loc_str = 'X'.
      ELSE.
        ls_data_unit-waers_ext_str = ls_data_unit-waers.
      ENDIF.

      WRITE ls_data_unit-wrbtr TO gc_dec_zero_c NO-GROUPING CURRENCY ls_data_unit-waers.
      ls_data_unit-wrbtr_str = gc_dec_zero_c.
      CONDENSE ls_data_unit-wrbtr_str.

*     (6.1.4) ####### ######### ######
      SELECT SINGLE * FROM t006a INTO ls_t006a
        WHERE spras = 'R'
          AND msehi = ls_data_unit-meins.
      IF sy-subrc = 0.
        CONCATENATE '(' ls_t006a-mseh6 ')' INTO lv_str.
        CONCATENATE lv_str ls_t006a-msehl  INTO lv_str SEPARATED BY space.

        ls_data_unit-meins_str = lv_str.
      ENDIF.

      MODIFY gt_data_unit01 FROM ls_data_unit.
    ENDLOOP.

  ELSE.
    APPEND INITIAL LINE TO gt_data_mat01.
    APPEND INITIAL LINE TO gt_data_unit01.
  ENDIF.

*** ######(######) --------------------------------------------------------------
  gt_data_mat02[] = it_data_group[].
  DELETE gt_data_mat02 WHERE trade_type NE gc_trade_type_02.
  IF lines( gt_data_mat02 ) > 0.

    LOOP AT gt_data_mat02 INTO ls_data_group.
      MOVE-CORRESPONDING: ls_data_group TO ls_data_unit.
      COLLECT ls_data_unit INTO gt_data_unit02.
    ENDLOOP.

    LOOP AT gt_data_unit02 INTO ls_data_unit.
*     (6.2.5) ######### ###### (######)
      IF ls_data_unit-waers = 'BYN'.
        ls_data_unit-waers_loc_str = 'X'.
      ELSE.
        ls_data_unit-waers_ext_str = ls_data_unit-waers.
      ENDIF.

      WRITE ls_data_unit-wrbtr TO gc_dec_zero_c NO-GROUPING CURRENCY ls_data_unit-waers.
      ls_data_unit-wrbtr_str = gc_dec_zero_c.
      CONDENSE ls_data_unit-wrbtr_str.

*     (6.2.6) #### ########## ###### (######## ######)
      CLEAR: lv_datum.
      LOOP AT gt_data_mat02 INTO ls_data_group WHERE matnr = ls_data_unit-matnr.
        IF ls_data_group-vatdate > lv_datum.
          lv_datum = ls_data_group-vatdate.
        ENDIF.
      ENDLOOP.
      IF lv_datum IS NOT INITIAL.
        ls_data_unit-vatdate = lv_datum.

        CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
          EXPORTING
            date_internal            = lv_datum
          IMPORTING
            date_external            = ls_data_unit-vatdate_str
          EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.
      ENDIF.

      MODIFY gt_data_unit02 FROM ls_data_unit.
    ENDLOOP.

  ELSE.
    APPEND INITIAL LINE TO gt_data_unit02.
  ENDIF.

*** ############# ##### ---------------------------------------------------------
  gt_data_mat03[] = it_data_group[].
  DELETE gt_data_mat03 WHERE trade_type NE gc_trade_type_03.
  IF lines( gt_data_mat03 ) > 0.

    LOOP AT gt_data_mat03 INTO ls_data_group.
      MOVE-CORRESPONDING: ls_data_group TO ls_data_unit.
      COLLECT ls_data_unit INTO gt_data_unit03.
    ENDLOOP.

    LOOP AT gt_data_unit03 INTO ls_data_unit.
*     (6.3.3) ######### ############## #####
      IF ls_data_unit-waers = 'BYN'.
        ls_data_unit-waers_loc_str = 'X'.
      ELSE.
        ls_data_unit-waers_ext_str = ls_data_unit-waers.
      ENDIF.

      WRITE ls_data_unit-wrbtr TO gc_dec_zero_c NO-GROUPING CURRENCY ls_data_unit-waers.
      ls_data_unit-wrbtr_str = gc_dec_zero_c.
      CONDENSE ls_data_unit-wrbtr_str.

      MODIFY gt_data_unit03 FROM ls_data_unit.
    ENDLOOP.

  ELSE.
    APPEND INITIAL LINE TO gt_data_unit03.
  ENDIF.

  " Body data
  set_xml_value 'BD' gs_str.

  " Tables
  set_xml_value 'T_MAT01'  gt_data_mat01.

  set_xml_value 'T_UNIT01' gt_data_unit01.
  set_xml_value 'T_UNIT02' gt_data_unit02.
  set_xml_value 'T_UNIT03' gt_data_unit03.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PRINT_OXML
*&---------------------------------------------------------------------*
FORM print_oxml USING iv_template_id TYPE zoxml_template_id
                      it_merge_table TYPE zoxml_merge_table.

  DATA: lo_merger  TYPE REF TO zcl_oxml_merger,
        lo_error   TYPE REF TO zcx_oxml_merger,
        lv_message TYPE string.

  DATA: ls_processing_options  TYPE zoxml_processing_options,
        ls_format_options      TYPE zoxml_format_options,
        lv_compressed_filename TYPE string,
        lv_compress_to_zip     TYPE abap_bool.

  DATA: lt_document      TYPE zoxml_binary_table,
        lv_document_size TYPE i.

  DATA: lv_uuid           TYPE sysuuid_c,
        lv_temp_dir       TYPE string,
        lv_filename(1024) TYPE c.

  " ######### ######### ############## ########
  ls_format_options-no_initial_values = abap_true.

  " ######### ########, ########## ###### # ###### ## ###########
  CREATE OBJECT lo_merger.
  TRY.
      lo_merger->merge(
      EXPORTING
        iv_template_id         = iv_template_id             " ############# ####### # ###########
        it_merge_table         = it_merge_table             " ####### ########### ########
        is_processing_options  = ls_processing_options      " ######### ######### ###### #######
        is_format_options      = ls_format_options          " ######### ############## ########
        iv_compressed_filename = lv_compressed_filename     " #### ####### ##### ######### ######### - ### ##### # ######
        iv_compress_to_zip     = abap_false                 " ##### ### ######### #########
      CHANGING
        ct_document            = lt_document                " ######## ########
        cv_document_size       = lv_document_size ).        " ###### ######### #########

    CATCH zcx_oxml_merger INTO lo_error.
      lv_message = lo_error->get_text( ).
      MESSAGE lv_message TYPE 'E'.
  ENDTRY.

* ######### #### ##### ### ##########
  CALL METHOD cl_gui_frontend_services=>get_temp_directory
    CHANGING
      temp_dir             = lv_temp_dir
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc = 0.
    CALL METHOD cl_gui_cfw=>flush.

    "UUID
    CALL FUNCTION 'SYSTEM_UUID_C_CREATE'
      IMPORTING
        uuid = lv_uuid.

    CONCATENATE lv_temp_dir '\' lv_uuid '.xml' INTO lv_filename.
  ENDIF.

* ######### ####
  IF lv_filename IS NOT INITIAL.
    CALL FUNCTION 'ZOXML_SAVE_LOCAL'
      EXPORTING
        iv_document_size = lv_document_size
        iv_extention     = 'XML'
        iv_filetype      = 'BIN'
        iv_execute       = 'X'
        iv_open_with     = 'E'
      CHANGING
        ct_document      = lt_document
        cv_file_name     = lv_filename.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SAVE_XML
*&---------------------------------------------------------------------*
FORM save_xml USING iv_msg_type    TYPE zedi_msg_type
                    it_merge_table TYPE zoxml_merge_table
                    it_data_group  TYPE tt_data
                    iv_set_number  TYPE n.

  DATA: lcl_msg      TYPE REF TO zedi_cl_messages,
        ls_data      TYPE REF TO data,
        lv_xml_data  TYPE xstring,
        lv_file_path TYPE string.

  FIELD-SYMBOLS: <fs_data> TYPE any.

  CREATE OBJECT lcl_msg
    EXPORTING
      iv_msg_type = iv_msg_type.

  CREATE DATA ls_data TYPE (lcl_msg->proxy_data_type).
  IF ls_data IS BOUND.
    ASSIGN ls_data->* TO <fs_data>.
  ENDIF.


  CASE iv_msg_type.
    WHEN gc_xml_form_doc.
      PERFORM get_xml_data_doc USING it_data_group
                                     it_merge_table
                            CHANGING <fs_data>.
    WHEN gc_xml_form_rat.
      PERFORM get_xml_data_rat USING it_data_group
                                     it_merge_table
                            CHANGING <fs_data>.
  ENDCASE.

  CALL METHOD lcl_msg->get_output_message
    EXPORTING
      is_msg_data          = <fs_data>
    IMPORTING
      ev_xml_data          = lv_xml_data
    EXCEPTIONS
      proxy_fault          = 1
      transformation_error = 2
      critical_error       = 3
      OTHERS               = 4.

  IF sy-subrc = 0.
    lv_file_path = p_xmldir.

    CASE iv_msg_type.
      WHEN gc_xml_form_doc.
        CONCATENATE lv_file_path 'DocumEconomValidCost' iv_set_number '.xml' INTO lv_file_path.
      WHEN gc_xml_form_rat.
        CONCATENATE lv_file_path 'EconomicSubstantiationAppliedPrice' iv_set_number '.xml' INTO lv_file_path.
    ENDCASE.

    CALL METHOD lcl_msg->download_msg_to_file
      EXPORTING
        iv_download_type            = zedi_if_file_load_type=>frontend
        iv_file_path                = lv_file_path
        iv_file_data                = lv_xml_data
      EXCEPTIONS
        invalid_file_path           = 1
        download_error              = 2
        download_type_not_supported = 3
        OTHERS                      = 4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_XML_DATA_DOC
*&---------------------------------------------------------------------*
*&      Service-Consumer CLASZFI_CO_DEVC_PROXY
*&---------------------------------------------------------------------*
FORM get_xml_data_doc USING it_data_group  TYPE tt_data
                            it_merge_table TYPE zoxml_merge_table
                   CHANGING cs_data        TYPE zfi_docum_econom_valid_cost.

  DATA: ls_merge_table  TYPE zoxml_merge_item,
        lt_data_collect TYPE tt_data,
        ls_data_collect TYPE ts_data,
        ls_data_group   TYPE ts_data,
        ls_data_unit    TYPE ts_data_unit.

  DATA: ls_section6_1 TYPE zfi_docum_econom_valid_cost_10,
        ls_section6_2 TYPE zfi_docum_econom_valid_cost_d8,
        ls_section6_3 TYPE zfi_docum_econom_valid_cost_d6,
        ls_table      TYPE zfi_docum_econom_valid_cost_11.

  FIELD-SYMBOLS: <ft_section6_1> TYPE zfi_docum_econom_valid_co_tab3,
                 <ft_section6_2> TYPE zfi_docum_econom_valid_co_tab2,
                 <ft_section6_3> TYPE zfi_docum_econom_valid_co_tab1,
                 <ft_table>      TYPE zfi_docum_econom_valid_co_tab4.

  FIELD-SYMBOLS: <fs_hd>     LIKE gs_header_data,
                 <fs_bd>     LIKE gs_str,
                 <ft_mat01>  LIKE gt_data_mat01,
                 <ft_unit01> LIKE gt_data_unit01,
                 <ft_unit02> LIKE gt_data_unit02,
                 <ft_unit03> LIKE gt_data_unit03.

  LOOP AT it_merge_table INTO ls_merge_table.
    CASE ls_merge_table-name.
      WHEN 'HD'.
        ASSIGN ls_merge_table-value_ptr->* TO <fs_hd>.
      WHEN 'BD'.
        ASSIGN ls_merge_table-value_ptr->* TO <fs_bd>.
      WHEN 'T_MAT01'.
        ASSIGN ls_merge_table-value_ptr->* TO <ft_mat01>.
      WHEN 'T_UNIT01'.
        ASSIGN ls_merge_table-value_ptr->* TO <ft_unit01>.
      WHEN 'T_UNIT02'.
        ASSIGN ls_merge_table-value_ptr->* TO <ft_unit02>.
      WHEN 'T_UNIT03'.
        ASSIGN ls_merge_table-value_ptr->* TO <ft_unit03>.
    ENDCASE.
  ENDLOOP.

* ########
  IF <fs_hd> IS ASSIGNED.
    cs_data-version  = '1'.
    cs_data-type     = 'DOCUMECONOMVALIDCOST'.
    cs_data-year     = <fs_hd>-gjahr.
    cs_data-kod_imns = <fs_hd>-zcode.
    cs_data-unp      = <fs_hd>-paval.

    cs_data-docum_econom_valid_cost_v1_f10-content = <fs_hd>-payer_name.
    cs_data-docum_econom_valid_cost_v1_f04-content = <fs_hd>-addr_info.

    cs_data-docum_econom_valid_cost_v1_f03-docum_econom_valid_cost_v1_f02 = <fs_hd>-zoked.
  ENDIF.

* ###### 1
  lt_data_collect[] = it_data_group[].
  SORT lt_data_collect BY cn_number cn_date.
  DELETE ADJACENT DUPLICATES FROM lt_data_collect COMPARING cn_number cn_date.

  IF lines( lt_data_collect ) > 1.
    cs_data-docum_econom_valid_cost_v1_p14-content = abap_true.

    LOOP AT lt_data_collect INTO ls_data_collect.
      cs_data-docum_econom_valid_cost_v1_p15-docum_econom_valid_cost_v1_p15 = ls_data_collect-cn_number.
      cs_data-docum_econom_valid_cost_v1_p15-docum_econom_valid_cost_v1_p14 = ls_data_collect-cn_date.
    ENDLOOP.
  ELSE.
    LOOP AT lt_data_collect INTO ls_data_collect.
      cs_data-docum_econom_valid_cost_v1_p15-docum_econom_valid_cost_v1_p17 = ls_data_collect-cn_number.
      cs_data-docum_econom_valid_cost_v1_p15-docum_econom_valid_cost_v1_p16 = ls_data_collect-cn_date.
    ENDLOOP.
  ENDIF.

  IF <fs_bd> IS ASSIGNED.
    cs_data-docum_econom_valid_cost_v1_p13-docum_econom_valid_cost_v1_p17 = <fs_bd>-str03.
    cs_data-docum_econom_valid_cost_v1_p13-docum_econom_valid_cost_v1_p16 = <fs_bd>-str04.
    cs_data-docum_econom_valid_cost_v1_p13-docum_econom_valid_cost_v1_p15 = <fs_bd>-str05.
    cs_data-docum_econom_valid_cost_v1_p13-docum_econom_valid_cost_v1_p14 = <fs_bd>-str06.
    cs_data-docum_econom_valid_cost_v1_p13-docum_econom_valid_cost_v1_p13 = <fs_bd>-str07.
    cs_data-docum_econom_valid_cost_v1_p13-docum_econom_valid_cost_v1_p12 = <fs_bd>-str08.
    cs_data-docum_econom_valid_cost_v1_p13-docum_econom_valid_cost_v1_p11 = <fs_bd>-str09.
    cs_data-docum_econom_valid_cost_v1_p13-docum_econom_valid_cost_v1_p1  = <fs_bd>-str10.

    cs_data-docum_econom_valid_cost_v1_p12-docum_econom_valid_cost_v1_p13 = <fs_bd>-str11.
    cs_data-docum_econom_valid_cost_v1_p12-docum_econom_valid_cost_v1_p12 = <fs_bd>-str12.
    cs_data-docum_econom_valid_cost_v1_p12-docum_econom_valid_cost_v1_p11 = <fs_bd>-str13.

    cs_data-docum_econom_valid_cost_v1_p11-content = '2'.

    cs_data-docum_econom_valid_cost_v1_p1-docum_econom_valid_cost_v1_p15 = gc_dec_zero.
    CONCATENATE <fs_bd>-str17 <fs_bd>-str18 <fs_bd>-str19 INTO cs_data-docum_econom_valid_cost_v1_p1-docum_econom_valid_cost_v1_p1.
  ENDIF.

* ###### 2
  IF <fs_bd> IS ASSIGNED.
    cs_data-docum_econom_valid_cost_v1_p23-docum_econom_valid_cost_v1_p21 = <fs_bd>-str20.
    cs_data-docum_econom_valid_cost_v1_p23-docum_econom_valid_cost_v1_p2  = <fs_bd>-str21.
    cs_data-docum_econom_valid_cost_v1_p2-content = <fs_bd>-str22.
  ENDIF.

* ###### 3
  cs_data-docum_econom_valid_cost_v1_p36-docum_econom_valid_cost_v1_p33 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p36-docum_econom_valid_cost_v1_p32 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p36-docum_econom_valid_cost_v1_p31 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p36-docum_econom_valid_cost_v1_p3  = gc_dec_zero.

  cs_data-docum_econom_valid_cost_v1_p35-docum_econom_valid_cost_v1_p33 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p35-docum_econom_valid_cost_v1_p32 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p35-docum_econom_valid_cost_v1_p31 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p35-docum_econom_valid_cost_v1_p3  = gc_dec_zero.

  cs_data-docum_econom_valid_cost_v1_p32-docum_econom_valid_cost_v1_p33 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p32-docum_econom_valid_cost_v1_p32 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p32-docum_econom_valid_cost_v1_p31 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p32-docum_econom_valid_cost_v1_p3  = gc_dec_zero.

  cs_data-docum_econom_valid_cost_v1_p3-docum_econom_valid_cost_v1_p33 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p3-docum_econom_valid_cost_v1_p32 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p3-docum_econom_valid_cost_v1_p31 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p3-docum_econom_valid_cost_v1_p3  = gc_dec_zero.

* ###### 5
  cs_data-docum_econom_valid_cost_v1_p5-content = gc_dec_zero.

* ###### 6.1
  IF <ft_unit01> IS ASSIGNED.
    LOOP AT <ft_unit01> INTO ls_data_unit.
      ASSIGN cs_data-docum_econom_valid_cost_v1_p63-docum_econom_valid_cost_v1_p6 TO <ft_section6_1>.
      IF <ft_section6_1> IS ASSIGNED.
        CLEAR: ls_section6_1.

        ls_section6_1-docum_econom_valid_cost_v1_p66 = ls_data_unit-sgtxt.
        ls_section6_1-docum_econom_valid_cost_v1_p65 = ls_data_unit-stawn.
        ls_section6_1-docum_econom_valid_cost_v1_p64 = ls_data_unit-wrbtr.
        ls_section6_1-docum_econom_valid_cost_v1_p63 = ls_data_unit-waers_loc_str.
        ls_section6_1-docum_econom_valid_cost_v1_p62 = ls_data_unit-waers_ext_str.
        ls_section6_1-docum_econom_valid_cost_v1_p61 = ls_data_unit-meins_str.

        ASSIGN ls_section6_1-docum_econom_valid_cost_v1_p6-docum_econom_valid_cost_v1_p6 TO <ft_table>.
        IF <ft_table> IS ASSIGNED.
          IF <ft_mat01> IS ASSIGNED.
            LOOP AT <ft_mat01> INTO ls_data_group.
              CLEAR: ls_table.

              ls_table-docum_econom_valid_cost_v1_p11 = ls_data_group-bldat.
              ls_table-docum_econom_valid_cost_v1_p67 = ls_data_group-netpr.
              ls_table-docum_econom_valid_cost_v1_p66 = ls_data_group-menge.
              ls_table-docum_econom_valid_cost_v1_p65 = ls_data_group-inco1.
              ls_table-docum_econom_valid_cost_v1_p10 = ls_data_group-country_origin.
              ls_table-docum_econom_valid_cost_v1_p68 = gc_dec_zero.

              APPEND ls_table TO <ft_table>.
            ENDLOOP.
          ENDIF.
        ENDIF.

        APPEND ls_section6_1 TO <ft_section6_1>.
      ENDIF.
    ENDLOOP.
  ENDIF.

* ###### 6.2
  IF <ft_unit02> IS ASSIGNED.
    LOOP AT <ft_unit02> INTO ls_data_unit.
      ASSIGN cs_data-docum_econom_valid_cost_v1_p62-docum_econom_valid_cost_v1_p6 TO <ft_section6_2>.
      IF <ft_section6_2> IS ASSIGNED.
        CLEAR: ls_section6_2.

        PERFORM get_okp USING ls_data_unit-stawn
                     CHANGING ls_section6_2-docum_econom_valid_cost_v1_p66.

        ls_section6_2-docum_econom_valid_cost_v1_p67 = ls_data_unit-sgtxt.
        ls_section6_2-docum_econom_valid_cost_v1_p63 = ls_data_unit-wrbtr.
        ls_section6_2-docum_econom_valid_cost_v1_p62 = ls_data_unit-waers_loc_str.
        ls_section6_2-docum_econom_valid_cost_v1_p61 = ls_data_unit-waers_ext_str.
        ls_section6_2-docum_econom_valid_cost_v1_p6  = ls_data_unit-vatdate.

        APPEND ls_section6_2 TO <ft_section6_2>.
      ENDIF.
    ENDLOOP.
  ENDIF.

* ###### 6.3
  IF <ft_unit03> IS ASSIGNED.
    LOOP AT <ft_unit03> INTO ls_data_unit.
      ASSIGN cs_data-docum_econom_valid_cost_v1_p61-docum_econom_valid_cost_v1_p6 TO <ft_section6_3>.
      IF <ft_section6_3> IS ASSIGNED.
        CLEAR: ls_section6_3.

        ls_section6_3-docum_econom_valid_cost_v1_p64 = ls_data_unit-sgtxt.
        ls_section6_3-docum_econom_valid_cost_v1_p62 = ls_data_unit-wrbtr.
        ls_section6_3-docum_econom_valid_cost_v1_p61 = ls_data_unit-waers_loc_str.
        ls_section6_3-docum_econom_valid_cost_v1_p6  = ls_data_unit-waers_ext_str.

        APPEND ls_section6_3 TO <ft_section6_3>.
      ENDIF.
    ENDLOOP.
  ENDIF.

* ###### 9
  cs_data-docum_econom_valid_cost_v1_p95-content = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p94-content = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p93-content = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p92-content = gc_dec_zero.

  cs_data-docum_econom_valid_cost_v1_p91-docum_econom_valid_cost_v1_p99 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p91-docum_econom_valid_cost_v1_p96 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p91-docum_econom_valid_cost_v1_p93 = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p91-docum_econom_valid_cost_v1_p9  = gc_dec_zero.

* ###### 12
  cs_data-docum_econom_valid_cost_v1_p19-content = gc_dec_zero.
  cs_data-docum_econom_valid_cost_v1_p20-content = gc_dec_zero.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_XML_DATA_RAT
*&---------------------------------------------------------------------*
*&      Service-Consumer CLASZFI_CO_ESAP_PROXY
*&---------------------------------------------------------------------*
FORM get_xml_data_rat USING it_data_group  TYPE tt_data
                            it_merge_table TYPE zoxml_merge_table
                   CHANGING cs_data        TYPE zfi_economic_substantion_appil.

  DATA: ls_merge_table  TYPE zoxml_merge_item,
        lt_data_collect TYPE tt_data,
        ls_data_collect TYPE ts_data,
        ls_data_group   TYPE ts_data,
        ls_data_unit    TYPE ts_data_unit.

  DATA: ls_group      TYPE zfi_document_with_price,
        ls_section4_1 TYPE zfi_section4_1,
        ls_section4_2 TYPE zfi_section4_2,
        ls_section4_3 TYPE zfi_section4_3,
        ls_table      TYPE zfi_table.

  FIELD-SYMBOLS: <ft_group>      TYPE zfi_document_with_price_tab,
                 <ft_section4_1> TYPE zfi_section4_1_tab,
                 <ft_section4_2> TYPE zfi_section4_2_tab,
                 <ft_section4_3> TYPE zfi_section4_3_tab,
                 <ft_table>      TYPE zfi_table_tab.

  FIELD-SYMBOLS: <fs_hd>     LIKE gs_header_data,
                 <fs_bd>     LIKE gs_str,
                 <ft_mat01>  LIKE gt_data_mat01,
                 <ft_unit01> LIKE gt_data_unit01,
                 <ft_unit02> LIKE gt_data_unit02,
                 <ft_unit03> LIKE gt_data_unit03.

  LOOP AT it_merge_table INTO ls_merge_table.
    CASE ls_merge_table-name.
      WHEN 'HD'.
        ASSIGN ls_merge_table-value_ptr->* TO <fs_hd>.
      WHEN 'BD'.
        ASSIGN ls_merge_table-value_ptr->* TO <fs_bd>.
      WHEN 'T_MAT01'.
        ASSIGN ls_merge_table-value_ptr->* TO <ft_mat01>.
      WHEN 'T_UNIT01'.
        ASSIGN ls_merge_table-value_ptr->* TO <ft_unit01>.
      WHEN 'T_UNIT02'.
        ASSIGN ls_merge_table-value_ptr->* TO <ft_unit02>.
      WHEN 'T_UNIT03'.
        ASSIGN ls_merge_table-value_ptr->* TO <ft_unit03>.
    ENDCASE.
  ENDLOOP.

* ########
  IF <fs_hd> IS ASSIGNED.
    cs_data-version  = '1'.
    cs_data-type     = 'ECONOMICSUBSTANTION'.
    cs_data-year     = <fs_hd>-gjahr.
    cs_data-kod_imns = <fs_hd>-zcode.
    cs_data-oked1    = <fs_hd>-zoked.
    cs_data-unp      = <fs_hd>-paval.
    cs_data-name     = <fs_hd>-payer_name.
    cs_data-address  = <fs_hd>-addr_info.
    cs_data-sign     = <fs_hd>-corrct.

    cs_data-payer_name     = <fs_hd>-payer_name.
    cs_data-payer_location = <fs_hd>-addr_info.
    cs_data-full_name      = <fs_hd>-fio.
  ENDIF.

* ###### 1
  lt_data_collect[] = it_data_group[].
  SORT lt_data_collect BY cn_number cn_date.
  DELETE ADJACENT DUPLICATES FROM lt_data_collect COMPARING cn_number cn_date.

  IF lines( lt_data_collect ) > 1.
    cs_data-section1-section1_2 = abap_true.

    ASSIGN cs_data-section1-section1_1-group TO <ft_group>.
    IF <ft_group> IS ASSIGNED.
      LOOP AT lt_data_collect INTO ls_data_collect.
        ls_group-document_name = '#######'.
        ls_group-number        = ls_data_collect-cn_number.

        PERFORM xml_convert_date USING ls_data_collect-cn_date
                              CHANGING ls_group-date.

        APPEND ls_group TO <ft_group>.
      ENDLOOP.
    ENDIF.
  ELSE.
    LOOP AT lt_data_collect INTO ls_data_collect.
      cs_data-section1-section1_1-contract-document_name = '#######'.
      cs_data-section1-section1_1-contract-number        = ls_data_collect-cn_number.

      PERFORM xml_convert_date USING ls_data_collect-cn_date
                            CHANGING cs_data-section1-section1_1-contract-date.

    ENDLOOP.
  ENDIF.

  IF <fs_bd> IS ASSIGNED.
    cs_data-section1-section1_3-sales_goods                 = <fs_bd>-str03.
    cs_data-section1-section1_3-implementation_work         = <fs_bd>-str04.
    cs_data-section1-section1_3-realization_property_rights = <fs_bd>-str05.
    cs_data-section1-section1_3-provision_property_for_use  = <fs_bd>-str06.
    cs_data-section1-section1_3-purchase_goods              = <fs_bd>-str07.
    cs_data-section1-section1_3-acquisition_work            = <fs_bd>-str08.
    cs_data-section1-section1_3-acquisition_property_rights = <fs_bd>-str09.
    cs_data-section1-section1_3-obtaining_property_for_use  = <fs_bd>-str10.

    cs_data-section1-section1_4-final_consumption = <fs_bd>-str11.
    cs_data-section1-section1_4-resale            = <fs_bd>-str12.
    cs_data-section1-section1_4-production_use    = <fs_bd>-str13.

    cs_data-section1-section1_5 = '2'.

    cs_data-section1-section1_6-prepayment = gc_dec_zero.
    CONCATENATE <fs_bd>-str17 <fs_bd>-str18 <fs_bd>-str19 INTO cs_data-section1-section1_6-other.
  ENDIF.

* ###### 2
  IF <fs_bd> IS ASSIGNED.
    cs_data-section2-section2_1-full_name = <fs_bd>-str20.
    cs_data-section2-section2_1-unp       = <fs_bd>-str21.
    cs_data-section2-section2_4           = <fs_bd>-str22.
  ENDIF.

* ###### 3
  cs_data-section3-section3_1-size_direct_participation_paye = gc_dec_zero.
  cs_data-section3-section3_1-size_direct_participation_seco = gc_dec_zero.
  cs_data-section3-section3_1-size_indirect_participation_pa = gc_dec_zero.
  cs_data-section3-section3_1-size_indirect_second_party     = gc_dec_zero.

  cs_data-section3-section3_2-size_direct_participation_paye = gc_dec_zero.
  cs_data-section3-section3_2-size_direct_participation_seco = gc_dec_zero.
  cs_data-section3-section3_2-size_indirect_participation_pa = gc_dec_zero.
  cs_data-section3-section3_2-size_indirect_second_party     = gc_dec_zero.

  cs_data-section3-section3_5-size_direct_participation_paye = gc_dec_zero.
  cs_data-section3-section3_5-size_direct_participation_seco = gc_dec_zero.
  cs_data-section3-section3_5-size_indirect_participation_pa = gc_dec_zero.
  cs_data-section3-section3_5-size_indirect_second_party     = gc_dec_zero.

  cs_data-section3-section3_7-size_direct_participation_paye = gc_dec_zero.
  cs_data-section3-section3_7-size_direct_participation_seco = gc_dec_zero.
  cs_data-section3-section3_7-size_indirect_participation_pa = gc_dec_zero.
  cs_data-section3-section3_7-size_indirect_second_party     = gc_dec_zero.

* ###### 4.1
  IF <ft_unit01> IS ASSIGNED.
    LOOP AT <ft_unit01> INTO ls_data_unit.
      ASSIGN cs_data-section4-section4_1 TO <ft_section4_1>.
      IF <ft_section4_1> IS ASSIGNED.
        CLEAR: ls_section4_1.

        ls_section4_1-section4_1_1                  = ls_data_unit-sgtxt.
        ls_section4_1-section4_1_2                  = ls_data_unit-stawn.
        ls_section4_1-section4_1_3-cost             = ls_data_unit-wrbtr.
        ls_section4_1-section4_1_3-ruble            = ls_data_unit-waers_loc_str.
        ls_section4_1-section4_1_3-foreign_currency = ls_data_unit-waers_ext_str.
        ls_section4_1-section4_1_4                  = ls_data_unit-meins_str.

        ASSIGN ls_section4_1-section4_1_5 TO <ft_table>.
        IF <ft_table> IS ASSIGNED.
          IF <ft_mat01> IS ASSIGNED.
            LOOP AT <ft_mat01> INTO ls_data_group.
              CLEAR: ls_table.

              PERFORM xml_convert_date USING ls_data_group-bldat
                                    CHANGING ls_table-shipment_date.

              ls_table-unit_price          = ls_data_group-netpr.
              ls_table-amount              = ls_data_group-menge.
              ls_table-delivery_terms      = ls_data_group-inco1.
              ls_table-country             = ls_data_group-country_origin.
              ls_table-implementation_name = gc_dec_zero.

              APPEND ls_table TO <ft_table>.
            ENDLOOP.
          ENDIF.
        ENDIF.

        APPEND ls_section4_1 TO <ft_section4_1>.
      ENDIF.
    ENDLOOP.
  ENDIF.

* ###### 4.2
  IF <ft_unit02> IS ASSIGNED.
    LOOP AT <ft_unit02> INTO ls_data_unit.
      ASSIGN cs_data-section4-section4_2 TO <ft_section4_2>.
      IF <ft_section4_2> IS ASSIGNED.
        CLEAR: ls_section4_2.

        PERFORM get_okp USING ls_data_unit-stawn
                     CHANGING ls_section4_2-section4_2_2.

        PERFORM xml_convert_date USING ls_data_unit-vatdate
                              CHANGING ls_section4_2-section4_2_6.

        ls_section4_2-section4_2_1                  = ls_data_unit-sgtxt.
        ls_section4_2-section4_2_5-cost             = ls_data_unit-wrbtr.
        ls_section4_2-section4_2_5-ruble            = ls_data_unit-waers_loc_str.
        ls_section4_2-section4_2_5-foreign_currency = ls_data_unit-waers_ext_str.

        APPEND ls_section4_2 TO <ft_section4_2>.
      ENDIF.
    ENDLOOP.
  ENDIF.

* ###### 4.3
  IF <ft_unit03> IS ASSIGNED.
    LOOP AT <ft_unit03> INTO ls_data_unit.
      ASSIGN cs_data-section4-section4_3 TO <ft_section4_3>.
      IF <ft_section4_3> IS ASSIGNED.
        CLEAR: ls_section4_3.

        ls_section4_3-section4_3_1                  = ls_data_unit-sgtxt.
        ls_section4_3-section4_3_3-cost             = ls_data_unit-wrbtr.
        ls_section4_3-section4_3_3-ruble            = ls_data_unit-waers_loc_str.
        ls_section4_3-section4_3_3-foreign_currency = ls_data_unit-waers_ext_str.

        APPEND ls_section4_3 TO <ft_section4_3>.
      ENDIF.
    ENDLOOP.
  ENDIF.

* ###### 5
  cs_data-section5-section5_1 = gc_dec_zero.
  cs_data-section5-section5_2 = gc_dec_zero.
  cs_data-section5-section5_3 = gc_dec_zero.
  cs_data-section5-section5_4 = gc_dec_zero.

  cs_data-section5-section5_5-section5_5_1-cost = gc_dec_zero.
  cs_data-section5-section5_5-section5_5_2-cost = gc_dec_zero.
  cs_data-section5-section5_5-section5_5_3-cost = gc_dec_zero.
  cs_data-section5-section5_5-section5_5_4      = gc_dec_zero.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_OKP
*&---------------------------------------------------------------------*
FORM get_okp USING iv_stawn TYPE stawn
          CHANGING cv_okp   TYPE string.

* ####  ############ ######                                                                                     ###
* 58290 ####### ####### ############ ###########                                                                58.29
* 61900 ###### ############ # ####### ################                                                          61.90.10.000
* 62010	############ # ####### ############# ################	                                                  62.01
* 62030 ############ ## ########## ############# #########                                                      62.03.1
* 63110	######### ######, ############## ##### ## ########## ########## # ######### # #### ############	        63.11.1
* 70220	################ ## ######## ############ ############ # ###### ################ ## ######## ##########	70.22.1
* 82110	############ ## ############## ########### ####### ################ #####	                              82.11.10.000

  CASE iv_stawn.
    WHEN '58290'. cv_okp = '58.29'.
    WHEN '61900'. cv_okp = '61.90.10.000'.
    WHEN '62010'. cv_okp = '62.01'.
    WHEN '62030'. cv_okp = '62.03.1'.
    WHEN '63110'. cv_okp = '63.11.1'.
    WHEN '63110'. cv_okp = '70.22.1'.
    WHEN '82110'. cv_okp = '82.11.10.000'.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  XML_CONVERT_DATE
*&---------------------------------------------------------------------*
FORM xml_convert_date USING iv_date     TYPE datum
                   CHANGING cv_datetime TYPE xsddatetime_z.

  CONVERT DATE iv_date TIME '000000' INTO TIME STAMP cv_datetime TIME ZONE 'UTC'.

ENDFORM.
