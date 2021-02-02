CLASS y_code_pal_buffer DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_buffer.
    ALIASES get FOR y_if_code_pal_buffer~get.
    ALIASES modify FOR y_if_code_pal_buffer~modify.
    ALIASES entry FOR y_if_code_pal_buffer~entry.

  PROTECTED SECTION.
    CLASS-METHODS update IMPORTING entry TYPE entry RAISING cx_sy_itab_line_not_found.
    CLASS-METHODS insert IMPORTING entry TYPE entry.

  PRIVATE SECTION.
    CONSTANTS max_entries TYPE i VALUE 100.
    CLASS-DATA internal TYPE y_if_code_pal_buffer~entries.

ENDCLASS.


CLASS y_code_pal_buffer IMPLEMENTATION.

  METHOD update.
    DATA(index) = line_index( internal[ object_type = entry-object_type
                                        object_name = entry-object_name ] ).
    internal[ index ] = entry.
  ENDMETHOD.

  METHOD insert.
    IF lines( internal ) > max_entries.
      DELETE internal FROM 1 TO max_entries / 2.
    ENDIF.

    internal = VALUE #( BASE internal
                      ( CORRESPONDING #( entry ) ) ).
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD modify.
    TRY.
        update( entry ).
      CATCH cx_sy_itab_line_not_found.
        insert( entry ).
    ENDTRY.
  ENDMETHOD.

  METHOD get.
    result = internal[ object_type = object_type
                       object_name = object_name ].
  ENDMETHOD.

ENDCLASS.
