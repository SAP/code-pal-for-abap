*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_select DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING destination TYPE rfcdest.
    METHODS set_table IMPORTING name TYPE string.
    METHODS set_sorted.
    METHODS add_column IMPORTING name TYPE string.

    METHODS add_where IMPORTING clause TYPE string.

    "! Run Query
    "! @parameter result | true - if record(s) found
    METHODS run RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_results EXPORTING result TYPE STANDARD TABLE.
    METHODS get_result EXPORTING result TYPE any.
    METHODS get_column IMPORTING name TYPE any
                       EXPORTING result TYPE any.

  PROTECTED SECTION.
    CONSTANTS delimiter VALUE ';'.

    DATA fields TYPE TABLE OF rfc_db_fld.
    DATA options TYPE TABLE OF rfc_db_opt.
    DATA destination TYPE rfcdest.
    DATA query_table TYPE dd02l-tabname.
    DATA get_sorted TYPE boole_d.
    DATA query_result TYPE sdti_result_tab.

ENDCLASS.



CLASS lcl_select IMPLEMENTATION.

  METHOD constructor.
    me->destination = destination.
  ENDMETHOD.

  METHOD set_table.
    query_table = name.
  ENDMETHOD.

  METHOD set_sorted.
    get_sorted = abap_true.
  ENDMETHOD.

  METHOD add_column.
    fields = VALUE #( BASE options ( CONV #( name ) ) ).
  ENDMETHOD.

  METHOD add_where.
    options = VALUE #( BASE options ( CONV #( clause ) ) ).
  ENDMETHOD.

  METHOD run.
    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION
        destination
      EXPORTING
        query_table          = query_table
        delimiter            = delimiter
        get_sorted           = get_sorted
        use_et_data_4_return = abap_true
      IMPORTING
        et_data              = query_result
      TABLES
        options              = options
        fields               = fields
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

  METHOD get_results.
    DATA object TYPE REF TO data.
    CREATE DATA object TYPE TABLE OF (query_table).
    ASSIGN object->* TO FIELD-SYMBOL(<table>).
    result = <table>.
  ENDMETHOD.

  METHOD get_result.
    DATA object TYPE REF TO data.
    CREATE DATA object TYPE (query_table).
    ASSIGN object->* TO FIELD-SYMBOL(<entry>).
    result = <entry>.
  ENDMETHOD.

  METHOD get_column.
    DATA object TYPE REF TO data.
    get_result( IMPORTING result = object ).

    READ TABLE fields INTO DATA(field) INDEX sy-index.


  ENDMETHOD.

ENDCLASS.


CLASS lcl_report_source DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING destination TYPE rfcdest
                                  object_type TYPE tadir-object
                                  object_name TYPE tadir-obj_name.

    "! Run Query
    "! @parameter result | true - if record(s) found
    METHODS run RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_source_code RETURNING VALUE(result) TYPE y_code_pal_database_access=>ty_source_code.
    METHODS get_trdir RETURNING VALUE(result) TYPE y_code_pal_database_access=>ty_trdir.

  PROTECTED SECTION.
    DATA destination TYPE rfcdest.
    DATA object_type TYPE tadir-object.
    DATA object_name TYPE tadir-obj_name.

  PRIVATE SECTION.
    DATA source_code TYPE y_code_pal_database_access=>ty_source_code.
    DATA trdir TYPE y_code_pal_database_access=>ty_trdir.

ENDCLASS.


CLASS lcl_report_source IMPLEMENTATION.

  METHOD constructor.
    me->destination = destination.
    me->object_type = object_type.
    me->object_name = object_name.
  ENDMETHOD.

  METHOD run.
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
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
