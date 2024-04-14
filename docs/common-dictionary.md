# Полезные ФМ, группы функций, классы
### Отображение BAL-лога
```
DATA: lt_return TYPE bapiret2_t.

CALL METHOD cl_rsdme_error=>display_log
  EXPORTING
    i_t_return = lt_return.
```
### Работа с фронтэндом
```
CLASS cl_gui_frontend_services DEFINITION LOAD.
  CALL METHOD cl_gui_frontend_services=>get_temp_directory
    CHANGING
      temp_dir     = lv_path
    EXCEPTIONS
      cntl_error   = 1
      error_no_gui = 2
      OTHERS       = 3.
```
### Работа с ZIP-архивами
Класс: ```CL_ABAP_ZIP```
### Утилиты для HTTP
Класс: ```CL_HTTP_UTILITY```
### Строковые переменные
``` CL_ABAP_CHAR_UTILITIES=>CR_LF ```

```
DATA: lv_c_nbcp TYPE c. 
lv_c_nbsp = cl_abap_conv_in_ce=>uccp( '00A0' ). “неразрывный пробел

IF cl_abap_matcher=>matches( pattern = '\d{23}'
                                text = l_string ) = abap_true.
   CONCATENATE l_string+0(9)
               l_string+9(4)
               l_string+13
          INTO l_string SEPARATED BY  gc_sep_dat.
ENDIF.
```
### Красивый вывод сообщений
```
CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = gt_messages.
```
### Чтение данных из документа Excel
ФМ ```ALSM_EXCEL_TO_INTERNAL_TABLE```
### Конвертация между XSTRING и BINARY и пр.
Группа функций ```SCMS_CONV```
### Получение метаданных
```
*&---------------------------------------------------------------------*
*&      Form  get_text_from_domain
*&---------------------------------------------------------------------*
*       Текст для постоянных значений домена
*----------------------------------------------------------------------*
FORM get_text_from_domain
         USING iv_value   TYPE any
         CHANGING ch_text TYPE any.

  DATA:
    li_tdescr TYPE REF TO cl_abap_typedescr ,
    li_edescr TYPE REF TO cl_abap_elemdescr ,
    lv_name   TYPE ddobjname ,
    ls_dd07v  TYPE dd07v ,
    lt_dd07v  TYPE STANDARD TABLE OF dd07v ,
    ls_dd04v  TYPE dd04v.

  CLEAR: ch_text.

* по ссылке - получаем элемент данных
  CALL METHOD cl_abap_datadescr=>describe_by_data
    EXPORTING
      p_data      = iv_value
    RECEIVING
      p_descr_ref = li_tdescr.

  li_edescr ?= li_tdescr.
  lv_name = li_edescr->help_id.

* по элементу данных - получаем домен
  CALL FUNCTION 'DDIF_DTEL_GET'
    EXPORTING
      name          = lv_name
      state         = 'A'
      langu         = ' '
    IMPORTING
      dd04v_wa      = ls_dd04v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

  CHECK sy-subrc EQ 0.
  lv_name = ls_dd04v-domname.

* получаем постоянные значения этого домена
  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = lv_name
      langu         = sy-langu
    TABLES
      dd07v_tab     = lt_dd07v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.

* среди них ищем текстовку к заданному значению
  CHECK sy-subrc EQ 0.
  READ TABLE lt_dd07v INTO ls_dd07v
    WITH KEY domvalue_l = iv_value.
  IF sy-subrc = 0.
    ch_text = ls_dd07v-ddtext.
  ENDIF.
ENDFORM.
```
### Перевод сумм во внутреннее представление
```
CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERN_9'
  EXPORTING
    currency             = ls_rest_partners-cn_waers
    amount_external      = lv_bapi_sum
    max_number_of_digits = 15
  IMPORTING
    amount_internal      = ls_rest_partners-cn_amount.
```
### Работа с курсами валют
http://www.sapnet.ru/viewtopic.php?t=103&highlight=currency+rate
### Запуск транзакции через ФМ
```
CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
  EXPORTING
    tcode                   = ev_tcode
    skip_screen             = abap_true
  TABLES
    spagpa_tab              = et_spagpa
  EXCEPTIONS
    call_transaction_denied = 1
    tcode_invalid           = 2
    OTHERS                  = 3.
```
### Генерация случайных чисел
```
DATA(lo_rand)  = cl_abap_random=>create( seed = cl_abap_random=>seed( ) ).
DATA(lv_value) = lo_rand->intinrange( low = 1 high = 10 ).
```
### Регулярные выражения
http://abap4.ru/regular-expression.html
```
IF cl_abap_matcher=>matches( pattern = '\d{23}'
                                text = l_string ) = abap_true.
     CONCATENATE l_string+0(9)
                 l_string+9(4)
                 l_string+13 l_string SEPARATED BY gc_sep_dat.
ENDIF.
```
### ADBC и Native SQL
```
TRY.
    DATA(lv_sql) = |SELECT spfli.carrid, spfli.connid|
                && |  FROM spfli|
                && |  WHERE spfli.mandt = '{ sy-mandt }' AND spfli.carrid = '{ p_carid }'|.

    DATA(lo_con) = cl_sql_connection=>get_connection( ).
    DATA(lo_sql) = lo_con->create_statement( ).
    DATA(lo_result) = lo_sql->execute_query( lv_sql ).

    GET REFERENCE OF lt_spfli INTO lr_spfli.
    lo_result->set_param_table( lr_spfli ).
    lo_result->next_package( ).
    lo_result->close( ).
    lo_con->close( ).

  CATCH cx_sql_exception INTO DATA(lx_sql_exc).
    DATA(lv_text) = lx_sql_exc->get_text( ).
    MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
ENDTRY.
```
### Работа с HASH-значением
Function group: ```SECH```

# Работа с Excel
OLE: ```zcl_edms_xls_epam_fi```, пример использования ```ZFICO_DOCS_UPLOAD_F01```, ```upload_excel_file```.
Минус: требует компьютер пользователя с Windows, могут быть проблемы при запуске из web
Особенность: работает только с тем, что видно на экране

Фон: ```zcl_fc_excel_reader```, пример использования: ```ZCL_EDMS_MASS_EPAM_FI_MULTI=>PARSE_INTERNAL()```.
Плюсы: можно работать в фоне, с несколькими вкладками
Минусы: не работает с xls-форматом (старый), не позволяет определить активную вкладку, считывает всю вкладку, вне зависимости от наличия скрытых строк.

Ещё есть ```cl_xlsx_document```, но я в ней не разобрался, нет времени.
Пример попытки: ```ZCL_FC_EXCEL_READER=>PREPARE```, закомментированные строки 22-30.

Есть ещё abap2xls: пакет ```ZABAP2XLSX```.
На сапборде есть xslx worbench Бородина и zwww (через олей) Parazit'а. И что-то ещё, нужно рыться.
Ещё есть iXML, вроде бы можно заюзать совместно с ```zcl_zip``` (или как его там).

# Дата / время
### Прибавить количество секунд к дате и времени
```
CALL FUNCTION 'TSTR_CALC_TIME'
  EXPORTING
    iv_begin_datelocal_req   = '2000101'
    iv_begin_timelocal_req   = '000000'
    iv_duration_integer      = lv_secs
  IMPORTING
    ev_end_datelocal         = lv_next_date
    ev_end_timelocal         = lv_next_time
  EXCEPTIONS
    fatal_error              = 1
    time_invalid             = 2
    time_missing             = 3
    tstream_not_loadable     = 4
    tstream_generation_error = 5
    parameter_error          = 6
    unspecified_error        = 7
    OTHERS                   = 8.
```
### Последний день месяца
```
CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = sy-datum
    IMPORTING
      e_date = lv_date.
```
### Работа с timestamp
```
GET TIME STAMP FIELD DATA(lv_ts).
DATA: lv_timestamp TYPE timestamp.

cl_abap_tstmp=>systemtstmp_syst2utc(
   EXPORTING
      syst_date = sy-datum
      syst_time = sy-uzeit
   IMPORTING
      utc_tstmp = lv_timestamp ).

CONVERT TIME STAMP lv_timestamp TIME ZONE 'UTC' INTO DATE DATA(lv_date) TIME DATA(lv_time).
```
### Классы для работы с датами
- ```CL_RECA_DATE```
- ```CL_RECA_DATE_SLICES```

# Экраны
### Чтение / запись данных с экрана
- ```DYNP_VALUES_READ```
- ```DYNP_VALUES_UPDATE```
### F4 для поля из таблицы
```
CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = 'SWOTENTRY'
      fieldname         = 'OBJTYPE'
    TABLES
      return_tab        = gt_ret_tab[]
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.
```
### Установить свой список значений для ListBox
```
DATA: lt_vrm_values TYPE vrm_values,
ls_vrm_values LIKE LINE OF lt_vrm_values.

ls_vrm_values-key  = 'FULL'.
ls_vrm_values-text = 'Полная налоговая ставка'.
APPEND ls_vrm_values TO lt_vrm_values.

ls_vrm_values-key  = 'FULL_20'.
ls_vrm_values-text = 'Полная налоговая ставка 20%'.
APPEND ls_vrm_values TO lt_vrm_values.

CALL FUNCTION 'VRM_SET_VALUES'
  EXPORTING
    id                = 'ZDF_CARD_999011-TAXGROUP'
    values          = lt_vrm_values
  EXCEPTIONS
    id_illegal_name = 1
    OTHERS            = 2.
```

# ALV
### Разворачивание ALV на полный экран
Программа ```ZDF_FIND_CARD_999011```, ```FORM init_alv```
### Простой вывод в ALV (1)
```
DATA: lr_alv TYPE REF TO cl_salv_table,
      lr_functions TYPE REF TO cl_salv_functions_list.

cl_salv_table=>factory( IMPORTING r_salv_table = lr_alv
                         CHANGING t_table = lt_data ).

lr_functions = lr_alv->get_functions( ).
lr_functions->set_all( ).

lr_alv->display( ).
```
### Простой вывод в ALV (2)
```
DATA(lo_ida) = cl_salv_gui_table_ida=>create( iv_table_name = 'ZTVLT_CRDNTL' ).

lo_ida->field_catalog( )->get_available_fields( IMPORTING ets_field_names = DATA(lt_fc) ).
DELETE lt_fc WHERE table_line EQ 'PWD'.
lo_ida->field_catalog( )->set_available_fields( lt_fc ).

lo_ida->fullscreen( )->display( ).
```
### Сброс буфера ALV-таблиц для всех системы
Программа ```BALVBUFDEL```

# Классы
### Вызов исключений
```
TRY.
    DATA(lv_uuid) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
  CATCH cx_uuid_error INTO DATA(lo_uuid_error).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
        previous          = lo_uuid_error
        message_unlimited = lo_uuid_error->get_longtext( ).
ENDTRY.
```

# Новый синтаксис
### Обход таблицы по группировкам
```
LOOP AT lt_data INTO DATA(ls_data)
         GROUP BY ( lifnr   = ls_data-lifnr
                    kunnr   = ls_data-kunnr
                    waers   = ls_data-waers
                    size = GROUP SIZE index = GROUP INDEX )
                  ASCENDING
                  REFERENCE INTO DATA(lr_group).

    REFRESH: lt_data_group.
    LOOP AT GROUP lr_group ASSIGNING FIELD-SYMBOL(<fs_data>).
      lt_data_group = VALUE #( BASE lt_data_group ( <fs_data> ) ).
    ENDLOOP.
ENDLOOP.
```
### Добавление строк в таблицу
```
ls_cdata = CORRESPONDING #( BASE ( ls_cdata ) <lfs_contract> 
     MAPPING sap_contract_n = doknr
  	           type = cn_type
  	        subtype = cn_subtype ).

lt_custcontr = CORRESPONDING #( BASE ( lt_vendcontr ) lt_cdata ).
```
### Простое alpha-преобразование
```
lv_kunnr = |{ gs_debitor-kunnr ALPHA = OUT }|.
```
### Работа с циклом FOR
```
gt_output = VALUE #( FOR <ls_initial_data> IN gt_initial_data ( fill_line( <ls_initial_data> ) ) ) .

DATA(ltr_matnr) = VALUE range_t_matnr( FOR <ls_bom_preview> IN lt_bom_preview_tab
                                         ( sign   = 'I'
                                           option = 'EQ'
                                           low    = CONV #( <ls_bom_preview>-matnr ) ) ).

DATA(lt_mseg_anla2) = VALUE ty_mseg( FOR ls_mseg IN gt_mseg WHERE ( anln1 IS NOT INITIAL ) ( ls_mseg ) ).
```
### Проверка наличия значения в таблице
```
IF line_exists( lt_dfs_filedata[ doctype = gc_doctype_51 ] ).
  CONTINUE.
ENDIF.
```
### Условие
```
<ls_data>-new_site = COND #( WHEN <ls_data>-cn_beg_date >= s_repdat-low THEN abap_true
                                                                        ELSE abap_false ).

<ls_data>-cn_revise_ao_dfs = COND #( WHEN ( lv_amnt - lv_measvalue ) 
                                       BETWEEN ( -1 * lc_ao_dfs_err ) AND lc_ao_dfs_err THEN icon_led_green
                                      		                                              ELSE icon_led_red ).
DATA(addrnumber) = SWITCH #( <ls_data>-zzre_obj_one_sid 
                      WHEN 1 THEN <ls_data>-addrnumber1
                      WHEN 2 THEN <ls_data>-addrnumber2
                      WHEN 3 THEN <ls_data>-addrnumber3
                      ELSE '' ).
```
### Расчет суммы по колонке таблицы
```
DATA: itab TYPE TABLE OF i WITH EMPTY KEY.
itab = VALUE #( FOR j = 1 WHILE j <= 10 ( j ) ).
DATA(sum) = REDUCE i( INIT x = 0 FOR wa IN itab NEXT x = x + wa ).
```
### Создание булевой переменной
```
DATA(result) = xsdbool( is_filled  = abap_true AND
                        is_working = abap_true ).
```

# Прочее
### ABAP DEMO package
```SABAPDEMOS```
### ABAP Git
```ZABAPGIT_STANDALONE```
### FICO common packages
- ```ZFICO_BC```
- ```ZLIB```
### Редактирование таблиц в продуктиве
- ```ZRK_SE16N``` – редактирование таблиц в продуктиве
- ```SE37``` - FM ```SE16N_INTERFACE```
### Печатные формы OAER, ZOAER
Функциональный модуль: ```Z_OUT_DOT```, ```Z_OUT_XLS```

- Приложение: ```kappl```
- Документ:   ```kschl```

Транзакция редактирования: ```ZOAER```

Транзакция создания видов выходных документов: ```NACT``` (```NACE```)
- Имя класса: ```classname```
- Тип класса: ```classtype```
- Ключ объекта: ```object_key```

Транзакция редактирования: ```OAER```

Создание нового класса: тр. ```SBDSV1```
### Поиск во всех текстах
- Программа ```RS_ABAP_SOURCE_SCAN```
- Транзакция ```CODE_SCANNER```
### Отладка фоновых заданий
```jdbg```
### Отладка под другим пользователем
1.	Пользователь, который совершает отладку, должен установить точку останова под другим пользователем.
   
    ![External user debugging](/assets/images/ext-user-debugging.png)

2.	Пользователь, под которым происходит отладка, запускает команду ```/hext user = EXTUSERNAME```.
### Создание системных вариантов
- ```CUS&``` - пользовательские транзакции
- ```SAP&``` - страндартные транзакции
### Работа с командами операционной системы
Создание команды – тр. ```SM69```
```
CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'SAPDBHOST'
                   ID 'VALUE' FIELD  lv_param.

lv_command = 'ZOR_PING'.
lv_param   = |-c{ pv_count } { lv_param }|.

CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
  EXPORTING
    commandname                   = lv_command
    additional_parameters         = lv_param
    operatingsystem               = 'ANYOS'
  IMPORTING
    status                        = lv_status
    exitcode                      = lv_exitcode
  TABLES
    exec_protocol                 = lt_protocol
  EXCEPTIONS
    no_permission                 = 1
    command_not_found             = 2
    parameters_too_long           = 3
    security_risk                 = 4
    wrong_check_call_interface    = 5
    program_start_error           = 6
    program_termination_error     = 7
    x_error                       = 8
    parameter_expected            = 9
    too_many_parameters           = 10
    illegal_command               = 11
    wrong_asynchronous_parameters = 12
    cant_enq_tbtco_entry          = 13
    jobcount_generation_error     = 14
    OTHERS                        = 15.

LOOP AT lt_protocol ASSIGNING FIELD-SYMBOL(<ls_protocol>).
  WRITE <ls_protocol>.
ENDLOOP.
```
### Добавление EXTENSION в ФМ BAPI_ACC_DOCUMENT_POST
FIBF – Параметры настройки – Модули процесса клиента

Процесс ```RWBAPI01```
### Массовая активация CDS
Program ```RUTDDLSACT```
### Массовая проверка синтаксиса
T-code ```REDSRS01```

# OData
Преобразование Filter string в select-options
```CL_CLB2_TOOLS=>ODATA_FILTER2SELECT_OPTION```

Преобразование select-options в WHERE-condition
```
CALL FUNCTION 'SE16N_CREATE_SELTAB'
    TABLES
      lt_sel   = gt_sel
      lt_where = gt_where.
```

# UI5
### Загрузка / выгрузка репозитория
Program ```/UI5/UI5_REPOSITORY_LOAD```
