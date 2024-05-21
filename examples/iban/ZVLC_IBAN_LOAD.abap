*&---------------------------------------------------------------------*
*& Report  ZVLC_IBAN_LOAD
*&---------------------------------------------------------------------*

REPORT zvlc_iban_load.

TYPE-POOLS: sabc, abap, truxs.

TABLES: lfbk.

TYPES:
      BEGIN OF ty_w_file_data,
       str01 TYPE string,
       str02 TYPE string,
       str03 TYPE string,
       str04 TYPE string,
       str05 TYPE string,
       str06 TYPE string,
       str07 TYPE string,
       str08 TYPE string,
       str09 TYPE string,
       str10 TYPE string,
      END OF ty_w_file_data,
      ty_t_file_data TYPE STANDARD TABLE OF ty_w_file_data,

      BEGIN OF ty_w_text,
       dataline(255) TYPE c,
      END OF ty_w_text,
      ty_t_text TYPE STANDARD TABLE OF ty_w_text,

      BEGIN OF ty_w_mess,
       dataline(255) TYPE c,
       type TYPE symsgty,
      END OF ty_w_mess,
      ty_t_mess TYPE STANDARD TABLE OF ty_w_mess,

      BEGIN OF ty_w_file,
       name     TYPE epsf-epsfilnam,
       dir      TYPE dxfields-longpath,
       dir_done TYPE dxfields-longpath,
       fullpath TYPE dxfields-longpath,
      END OF ty_w_file,
      ty_t_file TYPE STANDARD TABLE OF ty_w_file,

      BEGIN OF ty_string,
       part1 TYPE symsgv,
       part2 TYPE symsgv,
       part3 TYPE symsgv,
       part4 TYPE symsgv,
      END OF ty_string.

DATA: gv_full_name TYPE eseftfront,
      gv_directory TYPE string,
      gv_receiver  TYPE somlreci1-receiver.

PARAMETERS: p_dwl TYPE xfeld RADIOBUTTON GROUP gr3 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-006.
SELECT-OPTIONS:
  so_banks FOR lfbk-banks,
  so_bankl FOR lfbk-bankl.
PARAMETERS:
  p_dpath  TYPE eseftfront.
SELECTION-SCREEN END OF BLOCK b3.

PARAMETERS: p_upl TYPE xfeld RADIOBUTTON GROUP gr3.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:
  p_fron   TYPE xfeld RADIOBUTTON GROUP gr1 MODIF ID loc USER-COMMAND uc_srv DEFAULT 'X', " Фронтэнд
  p_upath  TYPE eseftfront MODIF ID frn,
  p_serv   TYPE xfeld RADIOBUTTON GROUP gr1 MODIF ID loc,                                 " Сервер
  p_dir    TYPE dxlpath MODIF ID srv LOWER CASE,
  p_dir_dn TYPE dxlpath MODIF ID srv LOWER CASE.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-007 NO INTERVALS.
PARAMETERS:
  p_fintr TYPE xfeld RADIOBUTTON GROUP gr2,             " Internal
  p_fnbrb TYPE xfeld RADIOBUTTON GROUP gr2 DEFAULT 'X'. " НБРБ
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002 NO INTERVALS.
PARAMETERS:
  p_email AS CHECKBOX USER-COMMAND flag. " Send e-mail
SELECT-OPTIONS:
  s_receiv FOR gv_receiver NO INTERVALS. " Receiver
SELECTION-SCREEN END OF BLOCK b2.
PARAMETERS:
  p_test AS CHECKBOX DEFAULT 'X'.        " Test run
SELECTION-SCREEN END OF BLOCK b1.

PARAMETERS: p_uplacc TYPE xfeld RADIOBUTTON GROUP gr3.
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-008.
PARAMETERS:
  p_apath TYPE eseftfront.
PARAMETERS:
  p_kr TYPE xfeld RADIOBUTTON GROUP gr4 DEFAULT 'X',
  p_db TYPE xfeld RADIOBUTTON GROUP gr4.
SELECTION-SCREEN END OF BLOCK b5.

*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upath.
  gv_full_name = p_upath.
  PERFORM file_dialog CHANGING gv_full_name.
  p_upath = gv_full_name.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dpath.
  gv_directory = p_dpath.
  PERFORM dir_dialog CHANGING gv_directory.
  p_dpath = gv_directory.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_apath.
  gv_full_name = p_apath.
  PERFORM file_dialog CHANGING gv_full_name.
  p_apath = gv_full_name.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_screen_options.

*&---------------------------------------------------------------------*
START-OF-SELECTION.

  CASE abap_true.
    WHEN p_dwl.
      PERFORM processing_dnl.
    WHEN p_upl.
      PERFORM processing_upl.
    WHEN p_uplacc.
      PERFORM processing_upl_acc.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  set_screen_options
*&---------------------------------------------------------------------*
FORM set_screen_options.

  IF p_serv = abap_true. " Directory for server is visible and for workstation is invisible
    LOOP AT SCREEN.
      IF screen-group1 = 'SRV'.
        screen-input = '1'.
        screen-invisible = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'FRN'.
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF p_fron = abap_true. " Directory for workstation is visible and for server is invisible
    LOOP AT SCREEN.
      IF screen-group1 = 'SRV'.
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'FRN'.
        screen-input = '1'.
        screen-invisible = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "set_screen_options

*&---------------------------------------------------------------------*
*&      Form  processing_dnl
*&---------------------------------------------------------------------*
FORM processing_dnl.

  CONSTANTS: lc_cur_code(3) TYPE c VALUE '933'.

  DATA: lt_data_file TYPE TABLE OF string,
        lt_data TYPE TABLE OF string,
        lv_data TYPE string,
        lv_filename TYPE string,
        lv_len TYPE i,
        lv_num(3) TYPE n.

  DATA: lt_lfbk TYPE TABLE OF lfbk WITH HEADER LINE,
        lt_knbk TYPE TABLE OF knbk WITH HEADER LINE.

* Выборка данных по кредиторам
  SELECT * FROM lfbk INTO TABLE lt_lfbk
    WHERE banks IN so_banks
      AND bankl IN so_bankl.

  LOOP AT lt_lfbk.
    CONDENSE lt_lfbk-bankl.
    SHIFT lt_lfbk-bankl LEFT DELETING LEADING '0'.
    lv_len = STRLEN( lt_lfbk-bankl ).
    IF lv_len NE 9.
      CONTINUE.
    ENDIF.
    CONDENSE lt_lfbk-bankn.
    SHIFT lt_lfbk-bankn LEFT DELETING LEADING '0'.
    lv_len = STRLEN( lt_lfbk-bankn ).
    IF lv_len NE 13.
      CONTINUE.
    ENDIF.

    CONCATENATE lt_lfbk-bankl ';' lc_cur_code ';' lt_lfbk-bankn INTO lv_data.
    APPEND lv_data TO lt_data.
  ENDLOOP.

* Выборка данных по дебиторам
  SELECT * FROM knbk INTO TABLE lt_knbk
      WHERE banks IN so_banks
        AND bankl IN so_bankl.

  LOOP AT lt_knbk.
    CONDENSE lt_knbk-bankl.
    SHIFT lt_knbk-bankl LEFT DELETING LEADING '0'.
    lv_len = STRLEN( lt_knbk-bankl ).
    IF lv_len NE 9.
      CONTINUE.
    ENDIF.
    CONDENSE lt_knbk-bankn.
    SHIFT lt_knbk-bankn LEFT DELETING LEADING '0'.
    lv_len = STRLEN( lt_knbk-bankn ).
    IF lv_len NE 13.
      CONTINUE.
    ENDIF.

    CONCATENATE lt_knbk-bankl ';' lc_cur_code ';' lt_knbk-bankn INTO lv_data.
    APPEND lv_data TO lt_data.
  ENDLOOP.

  SORT lt_data.
  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING ALL FIELDS.

* Запишем данные в файлы по 10.000 записей
  WHILE LINES( lt_data ) > 0.
    REFRESH: lt_data_file.

    lv_num = lv_num + 1.
    CONCATENATE p_dpath 'account_data_' lv_num '.csv' INTO lv_filename.

    APPEND LINES OF lt_data FROM 1 TO 10000 TO lt_data_file.
    DELETE lt_data FROM 1 TO 10000.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = lv_filename
      CHANGING
        data_tab                = lt_data_file
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.

    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDWHILE.

ENDFORM.                    "processing_dnl

*&---------------------------------------------------------------------*
*&      Form  processing_upl
*&---------------------------------------------------------------------*
FORM processing_upl.

  DATA: lt_file_data TYPE ty_t_file_data,
        lw_file_data TYPE ty_w_file_data.

  DATA: lt_mess TYPE ty_t_mess,
        lw_mess TYPE ty_w_mess.

  DATA: lv_cnt_run TYPE i,
        lv_cnt_str TYPE string,
        lv_lines   LIKE sy-tfill,
        lv_tabix   LIKE sy-tabix.

  DATA: lw_receiv   LIKE LINE OF s_receiv,
        lt_receiver TYPE ty_t_text,
        lw_receiver TYPE ty_w_text.

* Загрузка файла
  CASE abap_true.
    WHEN p_serv.
      PERFORM load_file_serv TABLES lt_file_data[] USING p_dir
                                                         p_dir_dn
                                                CHANGING lt_mess.
    WHEN p_fron.
      PERFORM load_file_fron TABLES lt_file_data[] USING p_upath.
  ENDCASE.

* Сначала грузим IBAN, у которых есть старый р/с
  IF p_fintr IS NOT INITIAL.
    SORT lt_file_data BY str01 ASCENDING str02 DESCENDING.
  ENDIF.

* Удалим дубликаты
  DELETE ADJACENT DUPLICATES FROM lt_file_data.

* Создание IBAN
  lv_lines = LINES( lt_file_data ).
  LOOP AT lt_file_data INTO lw_file_data.
    lv_tabix = sy-tabix.

    PERFORM create_iban USING lw_file_data
                              lv_tabix
                     CHANGING lt_mess.

    PERFORM progress_indicator USING 'Обработано записей:' lv_tabix lv_lines.

    lv_cnt_run = lv_cnt_run + 1.
  ENDLOOP.
  lv_cnt_str = lv_cnt_run.

  lw_mess-type = 'S'.
  CONCATENATE 'Загрузка завершена! Обработано записей:' lv_cnt_str INTO lw_mess-dataline SEPARATED BY space.
  PERFORM add_mess USING lw_mess CHANGING lt_mess.

* Отправка e-mail
  IF p_email = abap_true.
    IF s_receiv IS NOT INITIAL.

      LOOP AT s_receiv INTO lw_receiv.
        lw_receiver-dataline = lw_receiv-low.
        APPEND lw_receiver TO lt_receiver.
      ENDLOOP.

      IF lt_receiver IS NOT INITIAL.
        PERFORM send_email USING lt_receiver
                        CHANGING lt_mess.
      ENDIF.
    ENDIF.
  ENDIF.

* Запись лога
  IF p_test IS INITIAL.
    PERFORM write_bal_log USING lt_mess.
  ENDIF.

* Печать сообщений
  PERFORM write_mess USING lt_mess.

ENDFORM.                    "processing_upl

*&---------------------------------------------------------------------*
*&      Form  create_iban
*&---------------------------------------------------------------------*
FORM create_iban USING iw_file_data TYPE ty_w_file_data
                       iv_tabix     LIKE sy-tabix
              CHANGING ct_mess      TYPE ty_t_mess.

  DATA: lv_unp   TYPE stcd1,
        lv_banks TYPE banks,
        lv_bankn TYPE bankn,
        lv_bankl TYPE bankl,
        lv_iban  TYPE iban,
        lv_swift TYPE swift,
        lv_lifnr TYPE lifnr,
        lv_kunnr TYPE kunnr,
        lv_bvtyp TYPE bvtyp.

  DATA: lv_bank_acc LIKE bapi1013_key-bank_acct,
        lv_iban_start_date TYPE datum.

  DATA: lt_lfa  TYPE TABLE OF lfa1 WITH HEADER LINE,
        lt_kna  TYPE TABLE OF kna1 WITH HEADER LINE,
        ls_knbk TYPE knbk,
        ls_lfbk TYPE lfbk,
        lt_knbk TYPE knbk OCCURS 0 WITH HEADER LINE,
        lt_lfbk TYPE lfbk OCCURS 0 WITH HEADER LINE.

  DATA: ls_tiban  TYPE tiban,
        lt_itiban TYPE TABLE OF itiban,
        ls_itiban TYPE itiban,
        lv_tabkey TYPE tiban-tabkey.

  DATA: lw_mess      TYPE ty_w_mess,
        ls_return    TYPE bapiret2,
        lv_subrc     LIKE sy-subrc,
        lv_post      TYPE xfeld,
        lv_count     TYPE i,
        lv_lines_lfa TYPE i,
        lv_lines_kna TYPE i,
        lv_tabix(10) TYPE n.

  DATA: lt_tab_data TYPE mass_wa_tabdata OCCURS 0 WITH HEADER LINE,
        lt_msg      LIKE massmsg         OCCURS 0 WITH HEADER LINE.

  lv_tabix = iv_tabix.
  lv_iban_start_date = zvlc_iban=>get_iban_start_date( ).

  CASE abap_true.
    WHEN p_fintr. "Внутренний формат файла
      lv_unp   = iw_file_data-str01.  " УНП
      lv_bankn = iw_file_data-str02.  " Расчетный счет
      lv_iban  = iw_file_data-str04.  " IBAN
      lv_swift = iw_file_data-str05.  " SWIFT
    WHEN p_fnbrb. "Формат файла НБРБ
      lv_bankn = iw_file_data-str03.  " Расчетный счет
      lv_iban  = iw_file_data-str05.  " IBAN
      lv_swift = iw_file_data-str04.  " SWIFT
  ENDCASE.
  lv_banks = lv_iban(2).              " Страна банка

* Ключ банка
  SELECT SINGLE bankl FROM bnka INTO lv_bankl
    WHERE banks = lv_banks
      AND swift = lv_swift.

* Проверки корректности данных
  IF lv_swift = 'UNDEFINED' OR lv_iban = 'UNDEFINED'.
    RETURN.
  ENDIF.
  IF lv_iban(2) NE lv_swift+4(2).
    lw_mess-type = 'E'.
    CONCATENATE 'УНП:' lv_unp 'IBAN:' lv_iban 'SWIFT:' lv_swift 'Не совпадает код страны!' INTO lw_mess-dataline SEPARATED BY space.
    PERFORM add_mess USING lw_mess CHANGING ct_mess.
    RETURN.
  ENDIF.
  IF lv_iban+4(4) NE lv_swift(4).
    lw_mess-type = 'E'.
    CONCATENATE 'УНП:' lv_unp 'IBAN:' lv_iban 'SWIFT:' lv_swift 'Не совпадает код банка!' INTO lw_mess-dataline SEPARATED BY space.
    PERFORM add_mess USING lw_mess CHANGING ct_mess.
    RETURN.
  ENDIF.
  IF lv_bankl IS INITIAL.
    lw_mess-type = 'E'.
    CONCATENATE 'УНП:' lv_unp 'SWIFT:' lv_swift 'Не найден ключ банка!' INTO lw_mess-dataline SEPARATED BY space.
    PERFORM add_mess USING lw_mess CHANGING ct_mess.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CHECK_IBAN'
    EXPORTING
      i_iban    = lv_iban
    EXCEPTIONS
      not_valid = 1
      OTHERS    = 2.

  IF sy-subrc NE 0.
    lw_mess-type = 'E'.
    CONCATENATE 'УНП:' lv_unp 'IBAN:' lv_iban 'IBAN некорректен!' INTO lw_mess-dataline SEPARATED BY space.
    PERFORM add_mess USING lw_mess CHANGING ct_mess.
    RETURN.
  ENDIF.

* Создание IBAN
  IF p_test IS INITIAL.
    IF lv_bankn IS NOT INITIAL. " ... если указан расчетный счет

*     Проверим актуален ли этот расчетный счет для нашей системы
      CLEAR: lv_count.
      SELECT COUNT(*) FROM lfbk INTO lv_count
        WHERE bankn = lv_bankn.
      IF lv_count IS INITIAL.
        SELECT COUNT(*) FROM knbk INTO lv_count
          WHERE bankn = lv_bankn.
      ENDIF.
      IF lv_count IS INITIAL.
        SELECT COUNT(*) FROM t012k INTO lv_count
          WHERE bankn = lv_bankn.
      ENDIF.

      IF lv_count IS NOT INITIAL.
*       Проверим существует ли для него IBAN
        CALL FUNCTION 'READ_IBAN_FROM_DB'
          EXPORTING
            i_banks = lv_banks
            i_bankl = lv_bankl
            i_bankn = lv_bankn
            i_bkont = ''
          IMPORTING
            e_subrc = lv_subrc.

        IF lv_subrc NE 0.
          lv_bank_acc = lv_bankn.

          CALL FUNCTION 'BAPI_IBAN_CREATE'
            EXPORTING
              bankcountry_create       = lv_banks
              bankkey_create           = lv_bankl
              bankaccountnumber_create = lv_bank_acc
              controlkey_create        = ''
              iban                     = lv_iban
              valid_from               = lv_iban_start_date
            IMPORTING
              return                   = ls_return.

*         В случае ошибки пишем в лог
          IF ls_return-type CA 'EA'.
            lw_mess-type = ls_return-type.
            CONCATENATE ls_return-message_v1 ls_return-message_v2
                        ls_return-message_v3 ls_return-message_v4 INTO lw_mess-dataline SEPARATED BY space.

            PERFORM add_mess USING lw_mess CHANGING ct_mess.
            ROLLBACK WORK.
          ELSE.
            COMMIT WORK AND WAIT.
          ENDIF.
        ELSE.
          lw_mess-type = 'W'.
          CONCATENATE 'IBAN для' lv_banks lv_bankl lv_bankn 'уже существует!' INTO lw_mess-dataline SEPARATED BY space.
          PERFORM add_mess USING lw_mess CHANGING ct_mess.
        ENDIF.
      ELSE.
        lw_mess-type = 'S'.
        CONCATENATE 'УНП:' lv_unp 'р/с:' lv_bankn 'в системе не найден!' INTO lw_mess-dataline SEPARATED BY space.
        PERFORM add_mess USING lw_mess CHANGING ct_mess.
      ENDIF.

    ELSE. " ... если расчетный счет не указан

      SELECT lifnr FROM lfa1 INTO CORRESPONDING FIELDS OF TABLE lt_lfa
        WHERE stcd1 = lv_unp.
      SELECT kunnr FROM kna1 INTO CORRESPONDING FIELDS OF TABLE lt_kna
        WHERE stcd1 = lv_unp.

      lv_lines_lfa = LINES( lt_lfa ).
      lv_lines_kna = LINES( lt_kna ).

      IF lt_lfa[] IS NOT INITIAL OR lt_kna[] IS NOT INITIAL.
        IF lv_lines_lfa > 1.
          lw_mess-type = 'E'.
          CONCATENATE 'УНП:' lv_unp 'Найдено более одного кредитора! IBAN для кредитора не создан!' INTO lw_mess-dataline SEPARATED BY space.
          PERFORM add_mess USING lw_mess CHANGING ct_mess.
        ENDIF.
        IF lv_lines_kna > 1.
          lw_mess-type = 'E'.
          CONCATENATE 'УНП:' lv_unp 'Найдено более одного дебитора! IBAN для дебитора не создан!' INTO lw_mess-dataline SEPARATED BY space.
          PERFORM add_mess USING lw_mess CHANGING ct_mess.
        ENDIF.

*       Проверим существование IBAN
        CLEAR: lv_count.
        SELECT SINGLE COUNT(*) FROM tiban INTO lv_count
          WHERE iban = lv_iban.

*       КРЕДИТОР
        IF lv_lines_lfa = 1 AND lv_count = 0.
          READ TABLE lt_lfa INDEX 1.
          lv_lifnr  = lt_lfa-lifnr.
          lv_tabkey = lt_lfa-lifnr.

          CLEAR: ls_tiban, ls_itiban. REFRESH: lt_itiban.
          " Create IBAN with technical account number
          CALL FUNCTION 'CREATE_IBAN_WITH_TECH_ACCNO'
            EXPORTING
              i_banks        = lv_banks
              i_bankl        = lv_bankl
              i_bankn        = ''
              i_bkont        = ''
              i_iban         = lv_iban
              i_valid_from   = lv_iban_start_date
              i_tabname      = 'LFBK'
              i_tabkey       = lv_tabkey
            IMPORTING
              e_tiban        = ls_tiban
            EXCEPTIONS
              iban_not_valid = 1
              OTHERS         = 2.

          IF ls_tiban IS NOT INITIAL.
            ls_lfbk-lifnr = lv_lifnr.
            ls_lfbk-banks = lv_banks.
            ls_lfbk-bankl = lv_bankl.
            ls_lfbk-bankn = ls_tiban-bankn.
            ls_lfbk-bkref = 'Загружен с вн.сайта'.

            SELECT COUNT(*) FROM lfbk INTO lv_bvtyp
              WHERE lifnr = lv_lifnr.

            lv_bvtyp = lv_bvtyp + 1.
            ls_lfbk-bvtyp = lv_bvtyp.
            CONDENSE ls_lfbk-bvtyp.
            APPEND ls_lfbk TO lt_lfbk.

            " Создадим номер счета в банковских данных контрагента
            CALL FUNCTION 'FIN_AP_AR_ADD_BANK'
              EXPORTING
                i_koart      = 'K'
                i_bankdata   = ls_lfbk
              IMPORTING
                e_returncode = lv_subrc.

            IF lv_subrc IS NOT INITIAL.
              ROLLBACK WORK.

              lw_mess-type = 'E'.
              CONCATENATE 'Кредитор:' lv_lifnr 'Ошибка создания расчетного счета!' INTO lw_mess-dataline SEPARATED BY space.
              PERFORM add_mess USING lw_mess CHANGING ct_mess.
            ELSE.
              MOVE-CORRESPONDING: ls_tiban TO ls_itiban. ls_itiban-kz = 'I'.
              APPEND ls_itiban TO lt_itiban.

              " Запишем IBAN
              CALL FUNCTION 'POST_IBAN'
                TABLES
                  itiban = lt_itiban.

              COMMIT WORK AND WAIT.
            ENDIF.
          ENDIF.
        ENDIF.

*       ДЕБИТОР
        IF lv_lines_kna = 1 AND lv_count = 0.
          READ TABLE lt_kna INDEX 1.
          lv_kunnr  = lt_kna-kunnr.
          lv_tabkey = lt_kna-kunnr.

          CLEAR: ls_tiban, ls_itiban. REFRESH: lt_itiban.
          " Create IBAN with technical account number
          CALL FUNCTION 'CREATE_IBAN_WITH_TECH_ACCNO'
            EXPORTING
              i_banks        = lv_banks
              i_bankl        = lv_bankl
              i_bankn        = ''
              i_bkont        = ''
              i_iban         = lv_iban
              i_valid_from   = lv_iban_start_date
              i_tabname      = 'KNBK'
              i_tabkey       = lv_tabkey
            IMPORTING
              e_tiban        = ls_tiban
            EXCEPTIONS
              iban_not_valid = 1
              OTHERS         = 2.

          IF ls_tiban IS NOT INITIAL.
            ls_knbk-kunnr = lv_kunnr.
            ls_knbk-banks = lv_banks.
            ls_knbk-bankl = lv_bankl.
            ls_knbk-bankn = ls_tiban-bankn.
            ls_knbk-bkref = 'Загружен с вн.сайта'.

            SELECT COUNT(*) FROM knbk INTO lv_bvtyp
              WHERE kunnr = lv_kunnr.

            lv_bvtyp = lv_bvtyp + 1.
            ls_knbk-bvtyp = lv_bvtyp.
            CONDENSE ls_knbk-bvtyp.
            APPEND ls_knbk TO lt_knbk.

            " Создадим номер счета в банковских данных контрагента
            CALL FUNCTION 'FIN_AP_AR_ADD_BANK'
              EXPORTING
                i_koart      = 'D'
                i_bankdata   = ls_knbk
              IMPORTING
                e_returncode = lv_subrc.

            IF lv_subrc IS NOT INITIAL.
              ROLLBACK WORK.

              lw_mess-type = 'E'.
              CONCATENATE 'Дебитор:' lv_kunnr 'Ошибка создания расчетного счета!' INTO lw_mess-dataline SEPARATED BY space.
              PERFORM add_mess USING lw_mess CHANGING ct_mess.
            ELSE.
              MOVE-CORRESPONDING: ls_tiban TO ls_itiban. ls_itiban-kz = 'I'.
              APPEND ls_itiban TO lt_itiban.

              " Запишем IBAN
              CALL FUNCTION 'POST_IBAN'
                TABLES
                  itiban = lt_itiban.

              COMMIT WORK AND WAIT.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.
        lw_mess-type = 'S'.
        CONCATENATE 'УНП:' lv_unp 'в системе не найден!' INTO lw_mess-dataline SEPARATED BY space.
        PERFORM add_mess USING lw_mess CHANGING ct_mess.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    "create_iban

*&---------------------------------------------------------------------*
*&      Form  processing_upl_acc
*&---------------------------------------------------------------------*
FORM processing_upl_acc.

  DATA: lt_file_data TYPE ty_t_file_data,
        lw_file_data TYPE ty_w_file_data.

  DATA: lt_mess TYPE ty_t_mess,
        lw_mess TYPE ty_w_mess.

  DATA: lv_cnt_run TYPE i,
        lv_cnt_str TYPE string,
        lv_lines   LIKE sy-tfill,
        lv_tabix   LIKE sy-tabix.

* Загрузка файла
  PERFORM load_file_fron TABLES lt_file_data[] USING p_apath.

* Удалим дубликаты
  DELETE ADJACENT DUPLICATES FROM lt_file_data.

* Создание IBAN
  lv_lines = LINES( lt_file_data ).
  LOOP AT lt_file_data INTO lw_file_data.
    lv_tabix = sy-tabix.

    PERFORM create_account USING lw_file_data
                                 lv_tabix
                        CHANGING lt_mess.

    PERFORM progress_indicator USING 'Обработано записей:' lv_tabix lv_lines.

    lv_cnt_run = lv_cnt_run + 1.
  ENDLOOP.
  lv_cnt_str = lv_cnt_run.

  lw_mess-type = 'S'.
  CONCATENATE 'Загрузка завершена! Обработано записей:' lv_cnt_str INTO lw_mess-dataline SEPARATED BY space.
  PERFORM add_mess USING lw_mess CHANGING lt_mess.

* Печать сообщений
  PERFORM write_mess USING lt_mess.

ENDFORM.                    "processing_upl_acc

*&---------------------------------------------------------------------*
*&      Form  create_account
*&---------------------------------------------------------------------*
FORM create_account USING iw_file_data TYPE ty_w_file_data
                          iv_tabix     LIKE sy-tabix
                 CHANGING ct_mess      TYPE ty_t_mess.

  DATA: lv_lifnr TYPE lfbk-lifnr,
        lv_kunnr TYPE knbk-kunnr,
        lv_banks TYPE lfbk-banks,
        lv_bankl TYPE lfbk-bankl,
        lv_bankn TYPE lfbk-bankn,
        lv_bkont TYPE lfbk-bkont,
        lv_bvtyp TYPE lfbk-bvtyp,
        lv_xezer TYPE lfbk-xezer,
        lv_bkref TYPE lfbk-bkref,
        lv_koinh TYPE lfbk-koinh.

  DATA: ls_knbk TYPE knbk,
        ls_lfbk TYPE lfbk.

  DATA: lw_mess      TYPE ty_w_mess,
        ls_messages  TYPE ebpp_messages,
        lt_messages  TYPE TABLE OF ebpp_messages,
        ls_return    TYPE bapiret2,
        lv_subrc     LIKE sy-subrc,
        lv_count     TYPE i,
        lv_tabix(10) TYPE n.

  lv_tabix = iv_tabix.

  CASE abap_true.
    WHEN p_kr. "КРЕДИТОР
      lv_lifnr  = iw_file_data-str01.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_lifnr
        IMPORTING
          output = lv_lifnr.

      lv_banks  = iw_file_data-str02.
      lv_bankl  = iw_file_data-str03.
      lv_bankn  = iw_file_data-str04.
      lv_bkont  = iw_file_data-str05.
      lv_bvtyp  = iw_file_data-str06.
      lv_xezer  = iw_file_data-str07.
      lv_bkref  = iw_file_data-str08.
      lv_koinh  = iw_file_data-str09.

*     Проверим существует ли этот расчетный счет в системе
      CLEAR: lv_count.
      SELECT COUNT(*) FROM lfbk INTO lv_count
        WHERE lifnr = lv_lifnr
          AND banks = lv_banks
          AND bankl = lv_bankl
          AND bankn = lv_bankn.
      IF sy-subrc = 0.
        lw_mess-type = 'E'.
        CONCATENATE 'Расчетный счет' lv_bankn 'для кредитора:' lv_lifnr 'уже существует!' INTO lw_mess-dataline SEPARATED BY space.
        PERFORM add_mess USING lw_mess CHANGING ct_mess.
        EXIT.
      ENDIF.

      ls_lfbk-lifnr = lv_lifnr.
      ls_lfbk-banks = lv_banks.
      ls_lfbk-bankl = lv_bankl.
      ls_lfbk-bankn = lv_bankn.
      ls_lfbk-bkont = lv_bkont.
      ls_lfbk-bvtyp = lv_bvtyp.
      ls_lfbk-xezer = lv_xezer.
      ls_lfbk-bkref = lv_bkref.
      ls_lfbk-koinh = lv_koinh.

      " Создадим номер счета в банковских данных контрагента
      CALL FUNCTION 'FIN_AP_AR_ADD_BANK'
        EXPORTING
          i_koart      = 'K'
          i_bankdata   = ls_lfbk
        IMPORTING
          e_returncode = lv_subrc
        TABLES
          t_messages   = lt_messages.

      IF lv_subrc IS NOT INITIAL.
        ROLLBACK WORK.

        lw_mess-type = 'E'.
        CONCATENATE 'Кредитор:' lv_lifnr 'Ошибка создания расчетного счета!' INTO lw_mess-dataline SEPARATED BY space.
        PERFORM add_mess USING lw_mess CHANGING ct_mess.

        LOOP AT lt_messages INTO ls_messages.
          lw_mess-type     = ls_messages-msgty.
          lw_mess-dataline = ls_messages-msgtx.
          PERFORM add_mess USING lw_mess CHANGING ct_mess.
        ENDLOOP.
      ELSE.
        COMMIT WORK AND WAIT.
      ENDIF.

    WHEN p_db. "ДЕБИТОР
      lv_kunnr  = iw_file_data-str01.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_kunnr
        IMPORTING
          output = lv_kunnr.

      lv_banks  = iw_file_data-str02.
      lv_bankl  = iw_file_data-str03.
      lv_bankn  = iw_file_data-str04.
      lv_bkont  = iw_file_data-str05.
      lv_bvtyp  = iw_file_data-str06.
      lv_xezer  = iw_file_data-str07.
      lv_bkref  = iw_file_data-str08.
      lv_koinh  = iw_file_data-str09.

*     Проверим существует ли этот расчетный счет в системе
      CLEAR: lv_count.
      SELECT COUNT(*) FROM knbk INTO lv_count
        WHERE kunnr = lv_kunnr
          AND banks = lv_banks
          AND bankl = lv_bankl
          AND bankn = lv_bankn.
      IF sy-subrc = 0.
        lw_mess-type = 'E'.
        CONCATENATE 'Расчетный счет' lv_bankn 'для дебитора:' lv_kunnr 'уже существует!' INTO lw_mess-dataline SEPARATED BY space.
        PERFORM add_mess USING lw_mess CHANGING ct_mess.
        EXIT.
      ENDIF.

      ls_knbk-kunnr = lv_kunnr.
      ls_knbk-banks = lv_banks.
      ls_knbk-bankl = lv_bankl.
      ls_knbk-bankn = lv_bankn.
      ls_knbk-bkont = lv_bkont.
      ls_knbk-bvtyp = lv_bvtyp.
      ls_knbk-xezer = lv_xezer.
      ls_knbk-bkref = lv_bkref.
      ls_knbk-koinh = lv_koinh.

      " Создадим номер счета в банковских данных контрагента
      CALL FUNCTION 'FIN_AP_AR_ADD_BANK'
        EXPORTING
          i_koart      = 'D'
          i_bankdata   = ls_knbk
        IMPORTING
          e_returncode = lv_subrc
        TABLES
          t_messages   = lt_messages.

      IF lv_subrc IS NOT INITIAL.
        ROLLBACK WORK.

        lw_mess-type = 'E'.
        CONCATENATE 'Дебитор:' lv_kunnr 'Ошибка создания расчетного счета!' INTO lw_mess-dataline SEPARATED BY space.
        PERFORM add_mess USING lw_mess CHANGING ct_mess.

        LOOP AT lt_messages INTO ls_messages.
          lw_mess-type     = ls_messages-msgty.
          lw_mess-dataline = ls_messages-msgtx.
          PERFORM add_mess USING lw_mess CHANGING ct_mess.
        ENDLOOP.
      ELSE.
        COMMIT WORK AND WAIT.
      ENDIF.
  ENDCASE.

ENDFORM.                    "create_account

*&---------------------------------------------------------------------*
*&      Form  file_dialog
*&---------------------------------------------------------------------*
FORM file_dialog CHANGING cv_path.

  DATA: lv_title(100) VALUE 'Введите имя файла'.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = cv_path
      mask             = ',CSV-файлы (*.csv),*.csv,Все файлы (*.*),*.*,.'
      mode             = 'O'
      title            = lv_title
    IMPORTING
      filename         = cv_path
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    IF sy-msgid IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    "file_dialog

*&---------------------------------------------------------------------*
*&      Form  dir_dialog
*&---------------------------------------------------------------------*
FORM dir_dialog CHANGING cv_path.

  DATA: lv_num(3) TYPE n.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = ''
      initial_folder       = cv_path
    CHANGING
      selected_folder      = cv_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF cv_path IS NOT INITIAL.
    lv_num = STRLEN( cv_path ) - 1.
    IF cv_path+lv_num <> '\'.
      CONCATENATE cv_path '\' INTO cv_path.
    ENDIF.
  ENDIF.

ENDFORM.                    "dir_dialog

*&---------------------------------------------------------------------*
*&      Form  load_file_fron
*&---------------------------------------------------------------------*
FORM load_file_fron TABLES ct_data_table USING iv_path TYPE eseftfront.

  DATA lv_c_file_name  TYPE string.
  DATA lt_file_content TYPE truxs_t_text_data.

  IF iv_path IS INITIAL.
    RETURN.
  ENDIF.
  lv_c_file_name = iv_path.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_c_file_name
    CHANGING
      data_tab                = lt_file_content
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    MESSAGE e030(zptpmm_reserv_reval).
*   'Error occured during the file uploading'
  ENDIF.

  CALL FUNCTION 'TEXT_CONVERT_CSV_TO_SAP'
    EXPORTING
      i_line_header        = ''
      i_tab_raw_data       = lt_file_content
    TABLES
      i_tab_converted_data = ct_data_table
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE e030(zptpmm_reserv_reval).
*   'Error occured during the file uploading'
  ENDIF.

ENDFORM.                    "load_file_fron

*&---------------------------------------------------------------------*
*&      Form  load_file_serv
*&---------------------------------------------------------------------*
FORM load_file_serv TABLES ct_data_table TYPE ty_t_file_data
                     USING iv_dir        TYPE dxfields-longpath
                           iv_dir_dn     TYPE dxfields-longpath
                  CHANGING ct_mess       TYPE ty_t_mess.

  DATA: lt_filename TYPE ty_t_file,
        lw_filename TYPE ty_w_file,
        lv_dir      TYPE dxfields-longpath,
        lv_dir_dn   TYPE dxfields-longpath.

  DATA: l_err   TYPE xfeld,
        lw_mess TYPE ty_w_mess.

  lv_dir    = iv_dir.
  lv_dir_dn = iv_dir_dn.

* Get files
  PERFORM check_dir_name CHANGING lv_dir.
  PERFORM check_dir_name CHANGING lv_dir_dn.

  PERFORM get_fnames_from_dir USING lv_dir
                                    lv_dir_dn
                           CHANGING lt_filename.

  IF lt_filename IS INITIAL.
    lw_mess-type = 'S'.
    CONCATENATE 'No files in directory'(005) lv_dir INTO lw_mess-dataline SEPARATED BY space.
    PERFORM add_mess USING    lw_mess
                     CHANGING ct_mess.
  ENDIF.

  LOOP AT lt_filename INTO lw_filename.
    IF lw_filename-name(4) = 'IBAN'.
*     Upload file
      PERFORM process_file_upload USING    lw_filename
                                  CHANGING ct_data_table[]
                                           ct_mess
                                           l_err.

      IF l_err = abap_true.
        RETURN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "load_file_serv

*&---------------------------------------------------------------------*
*&      Form  check_dir_name
*&---------------------------------------------------------------------*
FORM check_dir_name CHANGING c_dir TYPE dxfields-longpath.

  DATA: l_slen TYPE i.

* Check '/' in the beginning of the path and in the end of the path
  CONDENSE c_dir.
  l_slen = STRLEN( c_dir ) - 1.
  IF c_dir+l_slen(1) NE '/'.
    CONCATENATE c_dir '/' INTO c_dir.
  ENDIF.
  IF c_dir+0(1) NE '/'.
    CONCATENATE '/' c_dir INTO c_dir.
  ENDIF.

ENDFORM.                    "check_dir_name

*&---------------------------------------------------------------------*
*&      Form  get_fnames_from_dir
*&---------------------------------------------------------------------*
FORM get_fnames_from_dir USING i_dir       TYPE dxfields-longpath
                               i_dir_dn    TYPE dxfields-longpath
                      CHANGING ct_filename TYPE ty_t_file.

  DATA:
        lt_fname    TYPE STANDARD TABLE OF epsfili,
        lw_fname    TYPE epsfili,
        lw_filename TYPE ty_w_file,
        l_dir       TYPE epsf-epsdirnam.

  l_dir = i_dir.

  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      dir_name               = l_dir
    TABLES
      dir_list               = lt_fname
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.

  IF sy-subrc EQ 0 AND lt_fname IS NOT INITIAL.
    LOOP AT lt_fname INTO lw_fname.
      lw_filename-name     = lw_fname-name.
      lw_filename-dir      = i_dir.
      lw_filename-dir_done = i_dir_dn.
      CONCATENATE lw_filename-dir lw_filename-name INTO lw_filename-fullpath.

      APPEND lw_filename TO ct_filename.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "get_fnames_from_dir

*&---------------------------------------------------------------------*
*&      Form  process_file_upload
*&---------------------------------------------------------------------*
FORM process_file_upload  USING    iw_filename  TYPE ty_w_file
                          CHANGING ct_filedata  TYPE ty_t_file_data
                                   ct_mess      TYPE ty_t_mess
                                   c_err        TYPE xfeld.

  DATA:
        l_dataset     TYPE string,
        l_dataset_new TYPE string,
        lw_import     TYPE ty_w_text,
        lw_filedata   TYPE ty_w_file_data,
        lw_mess       TYPE ty_w_mess,
        l_path        TYPE authb-filename.

  CLEAR c_err.

  l_dataset = iw_filename-fullpath.
  l_path    = iw_filename-dir.

  CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
    EXPORTING
      activity         = sabc_act_delete
      filename         = l_path
    EXCEPTIONS
      no_authority     = 1
      activity_unknown = 2
      OTHERS           = 3.
  IF sy-subrc NE 0.
    lw_mess-type = 'E'.
    MESSAGE e011(zotcsd) WITH l_path INTO lw_mess-dataline.
    PERFORM add_mess USING    lw_mess
                     CHANGING ct_mess.
    c_err = abap_true.
    RETURN.
  ENDIF.

  l_path = iw_filename-dir_done.

  CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
    EXPORTING
      activity         = sabc_act_write
      filename         = l_path
    EXCEPTIONS
      no_authority     = 1
      activity_unknown = 2
      OTHERS           = 3.
  IF sy-subrc NE 0.
    lw_mess-type = 'E'.
    MESSAGE e011(zotcsd) WITH l_path INTO lw_mess-dataline.
    PERFORM add_mess USING    lw_mess
                     CHANGING ct_mess.
    c_err = abap_true.
    RETURN.
  ENDIF.

* Open file for reading
  OPEN DATASET l_dataset FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    lw_mess-type = 'E'.
    MESSAGE e008(zotcsd) WITH l_dataset INTO lw_mess-dataline.
    PERFORM add_mess USING    lw_mess
                     CHANGING ct_mess.
    c_err = abap_true.
    RETURN.
  ENDIF.

  IF p_test EQ abap_false AND iw_filename-dir NE iw_filename-dir_done.
*   Open file for writing
    CONCATENATE iw_filename-dir_done iw_filename-name INTO l_dataset_new.
    OPEN DATASET l_dataset_new FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      lw_mess-type = 'E'.
      MESSAGE e008(zotcsd) WITH l_dataset_new INTO lw_mess-dataline.
      PERFORM add_mess USING    lw_mess
                       CHANGING ct_mess.
    ENDIF.
  ENDIF.
* Read file and write data to a new file
  DO.
    READ DATASET l_dataset INTO lw_import-dataline.
    IF sy-subrc EQ 0.
      CLEAR lw_filedata.
      SPLIT lw_import-dataline AT ';' INTO lw_filedata-str01
                                           lw_filedata-str02
                                           lw_filedata-str03
                                           lw_filedata-str04
                                           lw_filedata-str05.
      APPEND lw_filedata TO ct_filedata.
      NEW-LINE.
      IF p_test EQ abap_false AND iw_filename-dir NE iw_filename-dir_done.
        TRANSFER lw_import TO l_dataset_new.
      ENDIF.
    ELSE. " end of file
      EXIT.
    ENDIF.
  ENDDO.
  CLOSE DATASET l_dataset.
  IF p_test EQ abap_false AND iw_filename-dir NE iw_filename-dir_done.
    CLOSE DATASET l_dataset_new.
    DELETE DATASET l_dataset.
    IF  sy-subrc NE 0.
      lw_mess-type = 'E'.
      MESSAGE e009(zotcsd) WITH l_dataset INTO lw_mess-dataline.
      PERFORM add_mess USING    lw_mess
                       CHANGING ct_mess.
    ENDIF.
  ENDIF.

ENDFORM.                    "process_file_upload

*&---------------------------------------------------------------------*
*&      Form  ADD_MESS
*&---------------------------------------------------------------------*
*       Add message to table of messages
*----------------------------------------------------------------------*
*      -->IW_MESS  message
*      <--CT_MESS  table of messages
*----------------------------------------------------------------------*
FORM add_mess  USING    iw_mess TYPE ty_w_mess
               CHANGING ct_mess TYPE ty_t_mess.

  APPEND iw_mess TO ct_mess.

ENDFORM.                    " ADD_MESS

*&---------------------------------------------------------------------*
*&      Form  write_mess
*&---------------------------------------------------------------------*
FORM write_mess USING it_mess TYPE ty_t_mess.

  DATA lw_mess TYPE ty_w_mess.

  LOOP AT it_mess INTO lw_mess.
    WRITE: / lw_mess-dataline.
  ENDLOOP.

ENDFORM.                    "write_mess

*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
FORM send_email USING    it_receiver TYPE ty_t_text
                CHANGING ct_mess     TYPE ty_t_mess.

  DATA:
        lt_text         TYPE ty_t_text,
        lw_text         TYPE ty_w_text,
        lw_mess         TYPE ty_w_mess,
        l_email_address TYPE ad_smtpadr,
        l_subject       TYPE so_obj_des,
        lw_receiver     TYPE ty_w_text,
        l_extdate       TYPE c LENGTH 10
        .
  DATA:
        lref_send_request TYPE REF TO cl_bcs,
        lref_document     TYPE REF TO cl_document_bcs,
        lref_sender       TYPE REF TO cl_sapuser_bcs,
        lref_recipient    TYPE REF TO if_recipient_bcs
        .

  LOOP AT ct_mess INTO lw_mess.
    lw_text-dataline = lw_mess-dataline.
    APPEND lw_text TO lt_text.
  ENDLOOP.

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal = sy-datum
    IMPORTING
      date_external = l_extdate.

  CONCATENATE 'Status of IBAN download'(003) l_extdate INTO l_subject
    SEPARATED BY space.
* Send e-mail
  TRY.
* create persistent send request
      lref_send_request = cl_bcs=>create_persistent( ).

      lref_document = cl_document_bcs=>create_document( i_type    = 'RAW'
                                                        i_text    = lt_text
                                                        i_subject = l_subject ).
      lref_send_request->set_document( lref_document ).
* get sender object
      lref_sender = cl_sapuser_bcs=>create( sy-uname ).
      lref_send_request->set_sender( lref_sender ).

      LOOP AT it_receiver INTO lw_receiver.
        l_email_address = lw_receiver-dataline.
        lref_recipient = cl_cam_address_bcs=>create_internet_address( l_email_address ).

        CALL METHOD lref_send_request->add_recipient
          EXPORTING
            i_recipient = lref_recipient
            i_express   = 'X'.
      ENDLOOP.
* set that we don't need a Return Status E-mail
      CALL METHOD lref_send_request->set_status_attributes
        EXPORTING
          i_requested_status = 'N'
          i_status_mail      = 'N'.
* send document
      lref_send_request->set_send_immediately( 'X' ).
      lref_send_request->send( ).

    CATCH cx_send_req_bcs .
      lw_mess-type = 'E'.
      lw_mess-dataline = 'Error of sending of e-mail.'(004).
      PERFORM add_mess USING    lw_mess
                       CHANGING ct_mess.
      RETURN.
    CATCH cx_document_bcs .
      lw_mess-type = 'E'.
      lw_mess-dataline = 'Error of sending of e-mail.'(004).
      PERFORM add_mess USING    lw_mess
                       CHANGING ct_mess.
      RETURN.
    CATCH cx_address_bcs .
      lw_mess-type = 'E'.
      lw_mess-dataline = 'Error of sending of e-mail.'(004).
      PERFORM add_mess USING    lw_mess
                       CHANGING ct_mess.
      RETURN.
  ENDTRY.

  COMMIT WORK.

ENDFORM.                    " SEND_EMAIL

*&---------------------------------------------------------------------*
*&      Form  write_bal_log
*&---------------------------------------------------------------------*
FORM write_bal_log USING it_mess TYPE ty_t_mess.

  DATA: lw_log        TYPE bal_s_log,
        l_log_handle  TYPE balloghndl,
        lt_log_handle TYPE bal_t_logh,
        lw_msg        TYPE bal_s_msg,
        lw_string     TYPE ty_string,
        lw_mess       TYPE ty_w_mess.

  lw_log-object     = 'ZRTRFA_1320_FI'.
  lw_log-subobject  = 'ZRTRFA_IBAN'.
  lw_log-aluser     = sy-uname.
  lw_log-alprog     = sy-repid.
  lw_log-altcode    = sy-tcode.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = lw_log
    IMPORTING
      e_log_handle            = l_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Ошибка записи сообщений в журнал!' TYPE 'I'.
  ENDIF.

  LOOP AT it_mess INTO lw_mess.
    lw_string = lw_mess-dataline.

    CLEAR: lw_msg.
    lw_msg-msgty = lw_mess-type.
    lw_msg-msgid = 'SY'.
    lw_msg-msgno = 499.
    lw_msg-msgv1 = lw_string-part1.
    lw_msg-msgv2 = lw_string-part2.
    lw_msg-msgv3 = lw_string-part3.
    lw_msg-msgv4 = lw_string-part4.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = l_log_handle
        i_s_msg          = lw_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      MESSAGE 'Ошибка записи сообщений в журнал!' TYPE 'I'.
    ENDIF.
  ENDLOOP.

  APPEND l_log_handle TO lt_log_handle.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_save_all       = 'X'
      i_t_log_handle   = lt_log_handle
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.

ENDFORM.                    "write_bal_log

*&---------------------------------------------------------------------*
*&      Form  progress_indicator
*&---------------------------------------------------------------------*
FORM progress_indicator USING p_string
                              p_tabix TYPE int4
                              p_total TYPE int4.

  DATA: lv_str(100),
        lv_tabix_str(20),
        lv_total_str(20),
        lv_perc TYPE i,
        lv_nmod   TYPE i.              " Gomza 13.09.2011

* выводим только для кратных 10 счетчиков ( Gomza 13.09.2011 )
  lv_nmod = p_tabix MOD 10.
  IF lv_nmod <> 0.
    EXIT.
  ENDIF.

  lv_perc = ( p_tabix * 100 ) / p_total.

  WRITE p_string TO lv_str.

* Добавляем к сообщению Создание договоров ХХ из YY
  WRITE p_tabix TO lv_tabix_str LEFT-JUSTIFIED NO-ZERO.
  WRITE p_total TO lv_total_str LEFT-JUSTIFIED NO-ZERO.
  CONCATENATE lv_str lv_tabix_str 'из' lv_total_str
         INTO lv_str SEPARATED BY space.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lv_perc
      text       = lv_str
    EXCEPTIONS
      OTHERS     = 1.

ENDFORM.                    "progress_indicator
