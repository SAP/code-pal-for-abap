*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_select DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS create IMPORTING destination   TYPE rfcdest
                         RETURNING VALUE(result) TYPE REF TO lcl_select.

    METHODS set_table IMPORTING name          TYPE string
                      RETURNING VALUE(result) TYPE REF TO lcl_select.

    METHODS set_sorted RETURNING VALUE(result) TYPE REF TO lcl_select.

    METHODS add_column IMPORTING name          TYPE string
                       RETURNING VALUE(result) TYPE REF TO lcl_select.

    METHODS add_where IMPORTING clause        TYPE string
                      RETURNING VALUE(result) TYPE REF TO lcl_select.

    METHODS run RETURNING VALUE(result) TYPE REF TO lcl_select
                RAISING cx_sy_itab_error.

    METHODS get_table RETURNING VALUE(result) TYPE sdti_result_tab.

    METHODS get_column IMPORTING name          TYPE string
                       RETURNING VALUE(result) TYPE string.

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

  METHOD create.
    result = NEW #( ).
    result->destination = destination.
  ENDMETHOD.

  METHOD set_table.
    query_table = name.
    result = me.
  ENDMETHOD.

  METHOD set_sorted.
    get_sorted = abap_true.
    result = me.
  ENDMETHOD.

  METHOD add_column.
    fields = VALUE #( BASE options ( CONV #( name ) ) ).
    result = me.
  ENDMETHOD.

  METHOD add_where.
    options = VALUE #( BASE options ( CONV #( clause ) ) ).
    result = me.
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

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_error.
    ENDIF.

    result = me.
  ENDMETHOD.

  METHOD get_table.
    result = query_result.
  ENDMETHOD.

  METHOD get_column.
    DATA(entry) = query_result[ 1 ]-line.
    SPLIT entry AT delimiter INTO TABLE DATA(columns).
    result = columns[ line_index( fields[ fieldname = name ] ) ].
  ENDMETHOD.

ENDCLASS.
