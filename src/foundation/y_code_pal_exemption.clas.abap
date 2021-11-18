CLASS y_code_pal_exemption DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_exemption.
    METHODS constructor IMPORTING database_access TYPE REF TO y_code_pal_database_access.

  PRIVATE SECTION.
    ALIASES buffer FOR y_if_code_pal_exemption~buffer.
    ALIASES max_entries FOR y_if_code_pal_exemption~max_entries.
    ALIASES database_access FOR y_if_code_pal_exemption~database_access.

    METHODS get_from_buffer IMPORTING object_type   TYPE trobjtype
                                      object_name   TYPE sobj_name
                            RETURNING VALUE(result) TYPE abap_bool
                            RAISING   cx_sy_itab_line_not_found.

    METHODS try_new_exemption IMPORTING object_type   TYPE trobjtype
                                        object_name   TYPE sobj_name
                              RETURNING VALUE(result) TYPE abap_bool.

    METHODS cleanup_buffer.

ENDCLASS.



CLASS y_code_pal_exemption IMPLEMENTATION.

  METHOD constructor.
    me->database_access = database_access.
  ENDMETHOD.


  METHOD y_if_code_pal_exemption~is_exempt.
    TRY.
        result = get_from_buffer( object_type  = object_type
                                  object_name  = object_name ).
      CATCH cx_sy_itab_line_not_found.
        result = try_new_exemption( object_type = object_type
                                    object_name = object_name ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_from_buffer.
    result = buffer[ object_type = object_type
                     object_name = object_name ]-is_exempted.
  ENDMETHOD.


  METHOD try_new_exemption.
    cleanup_buffer( ).

    result = NEW lcl_exemption_general( database_access )->is_exempt( object_type = object_type
                                                                      object_name = object_name ).

    APPEND VALUE #( object_type = object_type
                    object_name = object_name
                    is_exempted = result ) TO buffer.
  ENDMETHOD.


  METHOD cleanup_buffer.
    CHECK lines( buffer ) > max_entries.
    DELETE buffer FROM 1 TO max_entries / 2.
  ENDMETHOD.

ENDCLASS.
