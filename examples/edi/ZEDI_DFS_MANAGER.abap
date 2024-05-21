*&-------------------------------------------------------------------------
* Program ID    : ZEDI_DFS_MANAGER
* Creation date : 15-Dec-2014
* Author        : Nikolay Kozlov
*--------------------------------------------------------------------------
*    Программа предназначена для обработки входящих / исходящих сообщений
* документов "Электронная накладная".
*
* History of changes
*--------------------------------------------------------------------------
*  Date      | User-ID   |  Description
*--------------------------------------------------------------------------
* 15.12.2014   Kozlov     Initial version
*--------------------------------------------------------------------------

REPORT zedi_dfs_manager.

INCLUDE zedi_dfs_manager_top.     " Global data

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_dname TYPE string LOWER CASE.
PARAMETERS: p_fname TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS:
  p_serv TYPE char1 RADIOBUTTON GROUP gr1 DEFAULT 'X' MODIF ID rb1 USER-COMMAND uc_srv,              " Server
  p_work TYPE char1 RADIOBUTTON GROUP gr1 MODIF ID rb1.                                              " Workstation
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_email AS CHECKBOX.
SELECT-OPTIONS: so_addr FOR adr6-smtp_addr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name EQ 'P_DNAME'.
      CASE abap_true.
        WHEN p_serv.
          PERFORM get_connection.
          screen-input = 0.
        WHEN p_work.
          CLEAR p_dname.
          screen-input = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

INITIALIZATION.

* Получим параметры подключения
  PERFORM get_connection.

START-OF-SELECTION.

* Завершаем работу программы, если существуют активные фоновые заданий с ее участием
  PERFORM check_running_jobs.

* Проверим заполнение пути к файлам
  IF p_dname IS INITIAL.
    EXIT.
  ENDIF.

* Получим список файлов
  REFRESH gt_files[].
  PERFORM get_file_list TABLES gt_files
                         USING p_dname p_fname.

  REFRESH gt_errors.

* Обработка файлов
  LOOP AT gt_files.
    CLEAR: gv_msg_type, gs_ret_default.

    FREE: gcl_msg, gs_msg_data.
    UNASSIGN <fs_cl_msg>.
    UNASSIGN <fs_gs_msg>.

*   Сформируем путь файла
    gv_file_name = gt_files-filename.
    CONCATENATE p_dname gt_files-filename INTO gv_file_path.
    CONCATENATE p_dname gt_files-filename INTO gv_file_path_archive.
    REPLACE gs_connect-repository_dir_in IN gv_file_path_archive WITH gs_connect-repository_dir_in_archive.

*   Получим тип сообщения
    PERFORM get_message_type USING gv_file_path
                          CHANGING gv_msg_type.

*   Обработка полученного типа сообщения
    IF gv_msg_type IS NOT INITIAL.
      CASE gv_msg_type.
        WHEN 'APERAK'.
          " Служебные сообщения не обрабатываем
          PERFORM move_file_to_archive USING gv_file_path
                                             gv_file_path_archive.
          CONTINUE.
        WHEN OTHERS.
          " Создадим класс обработки типа сообщения
          CREATE OBJECT gcl_msg
            EXPORTING
              iv_msg_type = gv_msg_type.
      ENDCASE.

      ASSIGN gcl_msg TO <fs_cl_msg>.
      CREATE DATA gs_msg_data TYPE (gcl_msg->proxy_data_type).
      IF gs_msg_data IS BOUND.
        ASSIGN gs_msg_data->* TO <fs_gs_msg>.
      ENDIF.

*     Обработка файла
      IF <fs_cl_msg> IS ASSIGNED AND <fs_gs_msg> IS ASSIGNED.
        PERFORM file_processing USING gv_msg_type
                                      gv_file_path
                                      gv_file_name
                                      <fs_cl_msg>
                             CHANGING <fs_gs_msg>
                                      gs_ret_default.
      ENDIF.

*     Если все в порядке, переместим файл в архив, иначе выводим сообщение
      IF gs_ret_default-type CA 'EA'.
        CONCATENATE 'Ошибка обработки файла:' gv_file_path '!' INTO gs_errors-line SEPARATED BY space.
        APPEND gs_errors TO gt_errors.

        gs_errors-line = gs_ret_default-message.
        APPEND gs_errors TO gt_errors.
      ELSE.
*       Переместим файл в архив
        PERFORM move_file_to_archive USING gv_file_path
                                           gv_file_path_archive.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Выводим сообщения
  IF lines( gt_errors ) > 0.
    LOOP AT gt_errors INTO gs_errors.
      WRITE: / gs_errors-line.
    ENDLOOP.

    IF p_email IS NOT INITIAL.
      PERFORM send_email.
    ENDIF.
  ENDIF.

  INCLUDE zedi_dfs_manager_forms.   " FORM-Routines
