CLASS y_code_pal_creation_date DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_creation_date.
    METHODS constructor IMPORTING database_access LIKE y_if_code_pal_creation_date~database_access.

  PROTECTED SECTION.
    ALIASES database_access FOR y_if_code_pal_creation_date~database_access.
    ALIASES buffer FOR y_if_code_pal_creation_date~buffer.
    ALIASES max_entries FOR y_if_code_pal_creation_date~max_entries.

  PRIVATE SECTION.
    METHODS get_from_buffer IMPORTING include       TYPE program
                            RETURNING VALUE(result) TYPE rdir_cdate
                            RAISING   cx_sy_itab_line_not_found.

    METHODS try_new_created_on IMPORTING include       TYPE program
                               RETURNING VALUE(result) TYPE as4date.

    METHODS cleanup_buffer.

ENDCLASS.



CLASS y_code_pal_creation_date IMPLEMENTATION.

  METHOD constructor.
    y_if_code_pal_creation_date~database_access = database_access.
  ENDMETHOD.

  METHOD y_if_code_pal_creation_date~get_creation_date.
    TRY.
        result = get_from_buffer( include ).
      CATCH cx_sy_itab_line_not_found.
        result = try_new_created_on( include ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_from_buffer.
    result = buffer[ include = include ]-created_on.
  ENDMETHOD.


  METHOD try_new_created_on.
    cleanup_buffer( ).

    DATA(trdir) = database_access->get_trdir( include ).
    SORT trdir BY cdat.

    result = trdir[ 1 ]-cdat.

    APPEND VALUE #( include    = include
                    created_on = result ) TO buffer.
  ENDMETHOD.


  METHOD cleanup_buffer.
    CHECK lines( buffer ) > max_entries.
    DELETE buffer FROM 1 TO max_entries / 2.
  ENDMETHOD.

ENDCLASS.
