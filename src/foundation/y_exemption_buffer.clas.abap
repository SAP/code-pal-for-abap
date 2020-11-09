CLASS y_exemption_buffer DEFINITION SHARED MEMORY ENABLED PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS modify IMPORTING exemption TYPE ytab_exemptions.
    CLASS-METHODS get IMPORTING object_type   TYPE ytab_exemptions-object
                                object_name   TYPE ytab_exemptions-obj_name
                      RETURNING VALUE(result) TYPE ytab_exemptions
                      RAISING cx_sy_itab_line_not_found.
  PROTECTED SECTION.
    CLASS-METHODS update IMPORTING exemption TYPE ytab_exemptions RAISING cx_sy_itab_line_not_found.
    CLASS-METHODS insert IMPORTING exemption TYPE ytab_exemptions.
  PRIVATE SECTION.
    CLASS-DATA internal TYPE TABLE OF ytab_exemptions.
ENDCLASS.


CLASS y_exemption_buffer IMPLEMENTATION.

  METHOD update.
    DATA(index) = line_index( internal[ object = exemption-object
                                        obj_name = exemption-obj_name ] ).
    internal[ index ] = exemption.
  ENDMETHOD.

  METHOD insert.
    internal = VALUE #( BASE internal
                      ( CORRESPONDING #( exemption ) ) ).
    ASSERT sy-subrc = 0.
  ENDMETHOD.

  METHOD modify.
    TRY.
        update( exemption ).
      CATCH cx_sy_itab_line_not_found.
        insert( exemption ).
    ENDTRY.
  ENDMETHOD.

  METHOD get.
    result = internal[ object = object_type
                       obj_name = object_name ].
  ENDMETHOD.

ENDCLASS.
