CLASS y_code_pal_scope DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_scope.
    METHODS constructor IMPORTING database_access LIKE y_if_code_pal_scope~database_access
                                  leading_include TYPE program.

  PRIVATE SECTION.
    ALIASES buffer FOR y_if_code_pal_scope~buffer.
    ALIASES max_entries FOR y_if_code_pal_scope~max_entries.
    ALIASES database_access FOR y_if_code_pal_scope~database_access.
    ALIASES leading_application_component FOR y_if_code_pal_scope~leading_application_component.

    METHODS get_from_buffer IMPORTING include TYPE program
                            RETURNING VALUE(result) TYPE abap_bool
                            RAISING   cx_sy_itab_line_not_found. "#EC METH_RET_BOOL

    METHODS get_from_database IMPORTING include TYPE program
                              RETURNING VALUE(result) TYPE abap_bool. "#EC METH_RET_BOOL

    METHODS get_application_component IMPORTING include TYPE program
                                      RETURNING VALUE(result) TYPE tadir-devclass.

    METHODS cleanup_buffer.

ENDCLASS.



CLASS y_code_pal_scope IMPLEMENTATION.

  METHOD constructor.
    y_if_code_pal_scope~database_access = database_access.
    y_if_code_pal_scope~leading_application_component = get_application_component( leading_include ).
  ENDMETHOD.


  METHOD y_if_code_pal_scope~is_it_in_scope.
    TRY.
        result = get_from_buffer( include ).
      CATCH cx_sy_itab_line_not_found.
        result = get_from_database( include ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_from_buffer.
    result = xsdbool( buffer[ include = include ]-application_component = leading_application_component ).
  ENDMETHOD.


  METHOD get_from_database.
    cleanup_buffer( ).

    DATA(application_component) = get_application_component( include ).

    APPEND VALUE #( include = include
                    application_component = application_component ) TO buffer.

    result = xsdbool( application_component = leading_application_component ).
  ENDMETHOD.


  METHOD get_application_component.
    DATA(object_key) = lcl_include_to_object_keys=>convert( include ).
    DATA(package) = database_access->repository_access->get_package_for_object_key( object_key ).
    result = package-app_comp-name.
  ENDMETHOD.


  METHOD cleanup_buffer.
    CHECK lines( buffer ) > max_entries.
    DELETE buffer FROM 1 TO max_entries / 2.
  ENDMETHOD.

ENDCLASS.
