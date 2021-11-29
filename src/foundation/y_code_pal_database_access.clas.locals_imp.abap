*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_select DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING rfc_destination TYPE rfcdest.
    METHODS set_from IMPORTING table TYPE string.
    METHODS add_where IMPORTING clause TYPE string.
    METHODS run CHANGING table TYPE table.

  PROTECTED SECTION.
    DATA rfc_destination TYPE rfcdest.
    DATA sql_where TYPE TABLE OF rfc_db_opt.
    DATA sql_from TYPE dd02l-tabname.

    METHODS run_local CHANGING table TYPE table.
    METHODS run_remote CHANGING table TYPE table.

ENDCLASS.



CLASS lcl_select IMPLEMENTATION.

  METHOD constructor.
    me->rfc_destination = rfc_destination.
  ENDMETHOD.

  METHOD set_from.
    sql_from = table.
  ENDMETHOD.

  METHOD add_where.
    sql_where = VALUE #( BASE sql_where ( CONV #( clause ) ) ).
  ENDMETHOD.

  METHOD run.
    IF rfc_destination IS INITIAL.
      run_local( CHANGING table = table ).
    ELSE.
      run_remote( CHANGING table = table ).
    ENDIF.
  ENDMETHOD.

  METHOD run_local.
    SELECT * FROM (sql_from) INTO TABLE @table WHERE (sql_where).
  ENDMETHOD.

  METHOD run_remote.
    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION
        rfc_destination
      EXPORTING
        sql_from             = sql_from
        use_et_data_4_return = abap_true
      IMPORTING
        et_data              = table
      TABLES
        sql_where            = sql_where
      EXCEPTIONS
        table_not_available   = 1
        table_without_data    = 2
        option_not_valid      = 3
        field_not_valid       = 4
        not_authorized        = 5
        data_buffer_exceeded  = 6
        system_failure        = 7
        communication_failure = 8
        resource_failure      = 9
        OTHERS                = 10.

    IF sy-subrc IS NOT INITIAL.
      CLEAR table.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_report_source DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING rfc_destination TYPE rfcdest
                                  object_type     TYPE versobjtyp
                                  object_name     TYPE versobjnam.

    "! Run Query
    "! @parameter result | true - if record(s) found
    METHODS run RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_source_code RETURNING VALUE(result) TYPE y_if_code_pal_database_access=>tty_source_code.

  PROTECTED SECTION.
    DATA rfc_destination TYPE rfcdest.
    DATA object_type TYPE versobjtyp.
    DATA object_name TYPE versobjnam.

  PRIVATE SECTION.
    DATA source_code TYPE y_if_code_pal_database_access=>tty_source_code.
    DATA trdir TYPE y_if_code_pal_database_access=>tty_trdir.

ENDCLASS.


CLASS lcl_report_source IMPLEMENTATION.

  METHOD constructor.
    me->rfc_destination = rfc_destination.
    me->object_type = object_type.
    me->object_name = object_name.
  ENDMETHOD.

  METHOD run.
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      DESTINATION
        rfc_destination
      EXPORTING
        object_name = object_name
        object_type = object_type
        versno      = 0
      TABLES
        repos_tab   = source_code
        trdir_tab   = trdir
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3
        resource_failure      = 4
        OTHERS                = 5.

    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD get_source_code.
    result = source_code.
  ENDMETHOD.

ENDCLASS.
