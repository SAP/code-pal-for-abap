CLASS y_code_pal_sorter DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_sorter.
    ALIASES get_position FOR y_if_code_pal_sorter~get_position.
  PRIVATE SECTION.
    CLASS-DATA tadir TYPE tt_tadir.
    CLASS-METHODS prepare.
    CLASS-METHODS determine IMPORTING check         TYPE sci_chk
                            RETURNING VALUE(result) TYPE synum03
                            RAISING   cx_sy_itab_line_not_found.
ENDCLASS.

CLASS y_code_pal_sorter IMPLEMENTATION.

  METHOD get_position.
    prepare( ).
    result = determine( check ).
  ENDMETHOD.

  METHOD prepare.
    CHECK tadir IS INITIAL.
    tadir = y_code_pal_profile=>get_checks_from_db( ).
  ENDMETHOD.

  METHOD determine.
    result = line_index( tadir[ obj_name = check ] ).
    IF result = -1.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
