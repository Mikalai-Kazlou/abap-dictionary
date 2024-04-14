*&---------------------------------------------------------------------*
*&  Include           ZEDI_DFS_MANAGER_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS: sabc, abap.

TABLES: adr6.

TYPES:
  BEGIN OF ty_xml_line,
    data(256) TYPE x,
  END OF ty_xml_line.
TYPES: tt_xml_line TYPE STANDARD TABLE OF ty_xml_line.

TYPES:
  BEGIN OF ty_file_list,
    filename TYPE string,
  END OF ty_file_list.
TYPES: tt_file_list TYPE STANDARD TABLE OF ty_file_list.

TYPES:
  BEGIN OF ts_errors,
    line TYPE string,
  END OF ts_errors.
TYPES: tt_errors TYPE STANDARD TABLE OF ts_errors.

TYPES:
  BEGIN OF ts_receivers_list,
    receiver_addr TYPE ad_smtpadr,
  END OF ts_receivers_list.
TYPES: tt_receivers_list TYPE TABLE OF ts_receivers_list.

* ѕередача описани€ и данных ALV-таблиц в EnhFM
TYPES: type_controls_table_enh TYPE /dfs/descr_alv.

DATA: gcl_msg              TYPE REF TO zedi_cl_messages,
      gs_msg_data          TYPE REF TO data,
      gv_msg_type          TYPE zedi_msg_type,
      gt_files             TYPE tt_file_list WITH HEADER LINE,
      gs_errors            TYPE ts_errors,
      gt_errors            TYPE tt_errors,
      gs_connect           TYPE zedi_connect,
      gv_file_name         TYPE string,
      gv_file_path         TYPE string,
      gv_file_path_archive TYPE string,
      gs_ret_default       TYPE bapiret2.

FIELD-SYMBOLS: <fs_cl_msg> TYPE REF TO zedi_cl_messages,
               <fs_gs_msg> TYPE any.

* «аполнение параметров в ret_default и выход при необходимости (&3 = 'X')
* ƒополнительно устанавливаетс€ класс сообщений и номер сообщени€
DEFINE macros_set_error.
  &1-id     = &4.
  &1-number = &5.
  &1-type   = &2.

  MESSAGE ID &1-id TYPE &1-type NUMBER &1-number INTO &1-message.

  IF &3 = 'X'.
    EXIT.
  ENDIF.
END-OF-DEFINITION.

* «аполнение параметров в ret_default и выход при необходимости (&3 = 'X')
* ƒополнительно устанавливаетс€ класс сообщений и номер сообщени€, а также параметры 1-4
DEFINE macros_set_error_param.
  &1-id      = &4.
  &1-number  = &5.
  &1-type    = &2.
  &1-message_v1 = &6.
  &1-message_v2 = &7.
  &1-message_v3 = &8.
  &1-message_v4 = &9.

  MESSAGE ID &1-id TYPE &1-type NUMBER &1-number WITH &1-message_v1
                                                      &1-message_v2
                                                      &1-message_v3
                                                      &1-message_v4 INTO &1-message.

  IF &3 = 'X'.
    EXIT.
  ENDIF.
END-OF-DEFINITION.
