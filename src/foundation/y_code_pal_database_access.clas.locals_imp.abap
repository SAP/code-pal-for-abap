*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_select DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING rfc_destination TYPE rfcdest.
    METHODS set_from IMPORTING table TYPE string.
    METHODS add_where IMPORTING clause TYPE string.

    "! Run Query
    "! @parameter result | true - if record(s) found
    METHODS run CHANGING table TYPE table
                RETURNING VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
    DATA rfc_destination TYPE rfcdest.
    DATA sql_where TYPE TABLE OF rfc_db_opt.
    DATA sql_from TYPE dd02l-tabname.

    METHODS run_local CHANGING table TYPE table
                      RETURNING VALUE(result) TYPE abap_bool.

    METHODS run_remote CHANGING table TYPE table
                       RETURNING VALUE(result) TYPE abap_bool.

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
    result = COND #( WHEN rfc_destination IS INITIAL THEN run_local( CHANGING table = table )
                                                     ELSE run_remote( CHANGING table = table ) ).
  ENDMETHOD.

  METHOD run_local.
    SELECT * FROM (sql_from) INTO TABLE table WHERE (sql_where).
    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD run_remote.
    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION
        rfc_destination
      EXPORTING
        sql_from             = sql_from
        use_et_data_4_return = abap_true
      IMPORTING
        et_data              = result
      TABLES
        sql_where            = sql_where
      EXCEPTIONS
        table_not_available  = 1
        table_without_data   = 2
        option_not_valid     = 3
        field_not_valid      = 4
        not_authorized       = 5
        data_buffer_exceeded = 6
        OTHERS               = 7.

    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_report_source DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING rfc_destination TYPE rfcdest
                                  object_type TYPE tadir-object
                                  object_name TYPE tadir-obj_name.

    "! Run Query
    "! @parameter result | true - if record(s) found
    METHODS run RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_source_code RETURNING VALUE(result) TYPE y_code_pal_database_access=>tty_source_code.
    METHODS get_trdir RETURNING VALUE(result) TYPE y_code_pal_database_access=>tty_trdir.

  PROTECTED SECTION.
    DATA rfc_destination TYPE rfcdest.
    DATA object_type TYPE tadir-object.
    DATA object_name TYPE tadir-obj_name.

  PRIVATE SECTION.
    DATA source_code TYPE y_code_pal_database_access=>tty_source_code.
    DATA trdir TYPE y_code_pal_database_access=>tty_trdir.

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
        no_version  = 1
        OTHERS      = 2.

    result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD get_source_code.
    result = source_code.
  ENDMETHOD.

  METHOD get_trdir.
    result = trdir.
  ENDMETHOD.

ENDCLASS.
