class ZVLC_IBAN definition
  public
  create public .

*"* public components of class ZVLC_IBAN
*"* do not include other source files here!!!
public section.

  data IBAN_START_DATE type DATUM read-only .

  methods CONSTRUCTOR .
  class-methods GET_IBAN_START_DATE
    returning
      value(EV_DATE) type DATUM
    exceptions
      DATA_NOT_FOUND .
  class-methods GET_IBAN_BY_BANK_ACCOUNT
    importing
      !IV_BANKS type BANKS default 'BY'
      !IV_BANKL type BANKL
      !IV_BANKN type BANKN
      !IV_BKONT type BKONT default ''
    exporting
      !EV_IBAN type IBAN
    exceptions
      DATA_NOT_FOUND .
  class-methods GET_BANK_ACCOUNT_BY_IBAN
    importing
      !IV_KOART type KOART
      !IV_IBAN type IBAN
    exporting
      !EV_BANKS type BANKS
      !EV_BANKL type BANKL
      !EV_BANKN type BANKN
      !EV_BKONT type BKONT
      !ET_LFBK_IBAN type ZVLC_LFBK_IBAN_T
      !ET_KNBK_IBAN type ZVLC_KNBK_IBAN_T
    exceptions
      DATA_NOT_FOUND .
  methods GET_IBAN_BANKL
    importing
      !IV_DATE type DATUM default SY-DATUM
      !IV_BANKS type BANKS default 'BY'
      !IV_BANKL type BANKL
    preferred parameter IV_DATE
    returning
      value(RV_BANKL) type ZLVC_IBAN_BANKL
    exceptions
      DATA_NOT_FOUND .
  methods GET_IBAN_BANKN
    importing
      !IV_DATE type DATUM default SY-DATUM
      !IV_BANKS type BANKS default 'BY'
      !IV_BANKL type BANKL
      !IV_BANKN type BANKN
      !IV_BKONT type BKONT default ''
    returning
      value(RV_BANKN) type ZLVC_IBAN_BANKN
    exceptions
      DATA_NOT_FOUND .
  methods GET_IBAN_BANKL_CC
    importing
      !IV_DATE type DATUM default SY-DATUM
      !IV_BUKRS type BUKRS
      !IV_HBKID type HBKID
    preferred parameter IV_DATE
    returning
      value(RV_BANKL) type ZLVC_IBAN_BANKL
    exceptions
      DATA_NOT_FOUND .
  methods GET_IBAN_BANKN_CC
    importing
      !IV_DATE type DATUM default SY-DATUM
      !IV_BUKRS type BUKRS
      !IV_HBKID type HBKID
      !IV_HKTID type HKTID
    returning
      value(RV_BANKN) type ZLVC_IBAN_BANKN
    exceptions
      DATA_NOT_FOUND .
protected section.
*"* protected components of class ZVLC_IBAN
*"* do not include other source files here!!!
private section.
*"* private components of class ZVLC_IBAN
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZVLC_IBAN IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZVLC_IBAN->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD constructor.

  me->iban_start_date = get_iban_start_date( ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZVLC_IBAN=>GET_BANK_ACCOUNT_BY_IBAN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_KOART                       TYPE        KOART
* | [--->] IV_IBAN                        TYPE        IBAN
* | [<---] EV_BANKS                       TYPE        BANKS
* | [<---] EV_BANKL                       TYPE        BANKL
* | [<---] EV_BANKN                       TYPE        BANKN
* | [<---] EV_BKONT                       TYPE        BKONT
* | [<---] ET_LFBK_IBAN                   TYPE        ZVLC_LFBK_IBAN_T
* | [<---] ET_KNBK_IBAN                   TYPE        ZVLC_KNBK_IBAN_T
* | [EXC!] DATA_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_bank_account_by_iban.

  DATA: lt_lfbk_iban TYPE TABLE OF zvlc_lfbk_iban,
        lt_knbk_iban TYPE TABLE OF zvlc_knbk_iban.

  DATA: ls_lfbk_iban TYPE zvlc_lfbk_iban,
        ls_knbk_iban TYPE zvlc_knbk_iban.

  CLEAR: ev_banks,
         ev_bankl,
         ev_bankn,
         ev_bkont.

  CASE iv_koart.
    WHEN 'K'. "Банковские данные для КРЕДИТОРА
      SELECT * FROM zvlc_lfbk_iban INTO TABLE lt_lfbk_iban
        WHERE iban = iv_iban.
      IF sy-subrc = 0.
        et_lfbk_iban[] = lt_lfbk_iban[].

        READ TABLE lt_lfbk_iban INTO ls_lfbk_iban INDEX 1.
        ev_banks = ls_lfbk_iban-banks.
        ev_bankl = ls_lfbk_iban-bankl.
        ev_bankn = ls_lfbk_iban-bankn.
        ev_bkont = ls_lfbk_iban-bkont.
      ELSE.
        RAISE data_not_found.
      ENDIF.
    WHEN 'D'. "Банковские данные для ДЕБИТОРА
      SELECT * FROM zvlc_knbk_iban INTO TABLE lt_knbk_iban
        WHERE iban = iv_iban.
      IF sy-subrc = 0.
        et_knbk_iban[] = lt_knbk_iban[].

        READ TABLE lt_knbk_iban INTO ls_knbk_iban INDEX 1.
        ev_banks = ls_knbk_iban-banks.
        ev_bankl = ls_knbk_iban-bankl.
        ev_bankn = ls_knbk_iban-bankn.
        ev_bkont = ls_knbk_iban-bkont.
      ELSE.
        RAISE data_not_found.
      ENDIF.
    WHEN OTHERS.
      RAISE data_not_found.
  ENDCASE.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZVLC_IBAN->GET_IBAN_BANKL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE                        TYPE        DATUM (default =SY-DATUM)
* | [--->] IV_BANKS                       TYPE        BANKS (default ='BY')
* | [--->] IV_BANKL                       TYPE        BANKL
* | [<-()] RV_BANKL                       TYPE        ZLVC_IBAN_BANKL
* | [EXC!] DATA_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_iban_bankl.

  CLEAR: rv_bankl.

  IF iv_date < me->iban_start_date.
    rv_bankl = iv_bankl.
  ELSE.
    SELECT SINGLE swift FROM bnka INTO rv_bankl
      WHERE banks = iv_banks
        AND bankl = iv_bankl.

    IF sy-subrc NE 0.
      RAISE data_not_found.
    ENDIF.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZVLC_IBAN->GET_IBAN_BANKL_CC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE                        TYPE        DATUM (default =SY-DATUM)
* | [--->] IV_BUKRS                       TYPE        BUKRS
* | [--->] IV_HBKID                       TYPE        HBKID
* | [<-()] RV_BANKL                       TYPE        ZLVC_IBAN_BANKL
* | [EXC!] DATA_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_iban_bankl_cc.

  DATA: lw_t012 TYPE t012.

  CLEAR: rv_bankl.

* Собственный банк
  CALL FUNCTION 'FI_HOUSEBANK_READ'
    EXPORTING
      ic_bukrs = iv_bukrs
      ic_hbkid = iv_hbkid
    IMPORTING
      es_t012  = lw_t012.

* Код собственного банка
  IF iv_date < me->iban_start_date.
    rv_bankl = lw_t012-bankl.
  ELSE.
    IF sy-subrc = 0.
      SELECT SINGLE swift FROM bnka INTO rv_bankl
        WHERE banks = lw_t012-banks
          AND bankl = lw_t012-bankl.
    ENDIF.

    IF sy-subrc NE 0.
      RAISE data_not_found.
    ENDIF.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZVLC_IBAN->GET_IBAN_BANKN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE                        TYPE        DATUM (default =SY-DATUM)
* | [--->] IV_BANKS                       TYPE        BANKS (default ='BY')
* | [--->] IV_BANKL                       TYPE        BANKL
* | [--->] IV_BANKN                       TYPE        BANKN
* | [--->] IV_BKONT                       TYPE        BKONT (default ='')
* | [<-()] RV_BANKN                       TYPE        ZLVC_IBAN_BANKN
* | [EXC!] DATA_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_iban_bankn.

  DATA: lv_iban TYPE iban.

  CLEAR: rv_bankn.

  IF iv_date < me->iban_start_date.
    rv_bankn = iv_bankn.
  ELSE.
    IF iv_banks = 'RU'. " Для РФ
      CONCATENATE iv_bkont iv_bankn INTO rv_bankn.
    ELSE.
      CALL METHOD me->get_iban_by_bank_account
        EXPORTING
          iv_banks       = iv_banks
          iv_bankl       = iv_bankl
          iv_bankn       = iv_bankn
          iv_bkont       = iv_bkont
        IMPORTING
          ev_iban        = lv_iban
        EXCEPTIONS
          data_not_found = 1
          OTHERS         = 2.

      IF sy-subrc NE 0.
        RAISE data_not_found.
      ELSE.
        rv_bankn = lv_iban.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZVLC_IBAN->GET_IBAN_BANKN_CC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE                        TYPE        DATUM (default =SY-DATUM)
* | [--->] IV_BUKRS                       TYPE        BUKRS
* | [--->] IV_HBKID                       TYPE        HBKID
* | [--->] IV_HKTID                       TYPE        HKTID
* | [<-()] RV_BANKN                       TYPE        ZLVC_IBAN_BANKN
* | [EXC!] DATA_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_iban_bankn_cc.

  DATA: lv_iban TYPE iban.

  DATA: lw_t012  TYPE t012,
        lw_t012k TYPE t012k.

  CLEAR: rv_bankn.

* Собственный расчетный счет
  CALL FUNCTION 'FI_HOUSEBANK_ACCOUNT_READ'
    EXPORTING
      ic_bukrs = iv_bukrs
      ic_hbkid = iv_hbkid
      ic_hktid = iv_hktid
    IMPORTING
      es_t012k = lw_t012k.

* IBAN
  IF iv_date < me->iban_start_date.
    rv_bankn = lw_t012k-bankn.
  ELSE.

*   Собственный банк
    CALL FUNCTION 'FI_HOUSEBANK_READ'
      EXPORTING
        ic_bukrs = iv_bukrs
        ic_hbkid = iv_hbkid
      IMPORTING
        es_t012  = lw_t012.

    CALL METHOD me->get_iban_by_bank_account
      EXPORTING
        iv_banks       = lw_t012-banks
        iv_bankl       = lw_t012-bankl
        iv_bankn       = lw_t012k-bankn
        iv_bkont       = lw_t012k-bkont
      IMPORTING
        ev_iban        = lv_iban
      EXCEPTIONS
        data_not_found = 1
        OTHERS         = 2.

    IF sy-subrc NE 0.
      RAISE data_not_found.
    ELSE.
      rv_bankn = lv_iban.
    ENDIF.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZVLC_IBAN=>GET_IBAN_BY_BANK_ACCOUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BANKS                       TYPE        BANKS (default ='BY')
* | [--->] IV_BANKL                       TYPE        BANKL
* | [--->] IV_BANKN                       TYPE        BANKN
* | [--->] IV_BKONT                       TYPE        BKONT (default ='')
* | [<---] EV_IBAN                        TYPE        IBAN
* | [EXC!] DATA_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_iban_by_bank_account.

  DATA: ls_iban_key    TYPE bapi1013_key,
        ls_iban_detail TYPE bapi1013_detail,
        ls_return      TYPE bapiret2.

  ls_iban_key-bank_ctry = iv_banks.
  ls_iban_key-bank_key  = iv_bankl.
  ls_iban_key-bank_acct = iv_bankn.
  ls_iban_key-ctrl_key  = iv_bkont.

  CALL FUNCTION 'BAPI_IBAN_GETDETAIL'
    EXPORTING
      bankcountry       = ls_iban_key-bank_ctry
      bankkey           = ls_iban_key-bank_key
      bankaccountnumber = ls_iban_key-bank_acct
      bankcontrolkey    = ls_iban_key-ctrl_key
    IMPORTING
      detail            = ls_iban_detail
      return            = ls_return.

  IF ls_return-type CA 'EA'.
    RAISE data_not_found.
  ELSE.
    ev_iban = ls_iban_detail-iban.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZVLC_IBAN=>GET_IBAN_START_DATE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EV_DATE                        TYPE        DATUM
* | [EXC!] DATA_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_iban_start_date.

  SELECT SINGLE iban_start_date FROM zvlc_iban_set INTO ev_date.

  IF sy-subrc NE 0.
    RAISE data_not_found.
  ENDIF.

ENDMETHOD.
ENDCLASS.
