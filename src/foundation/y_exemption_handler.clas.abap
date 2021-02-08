CLASS y_exemption_handler DEFINITION  PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES y_if_exemption.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_buffer,
           object_type TYPE trobjtype,
           object_name TYPE trobj_name,
           is_exempted TYPE abap_bool,
         END OF ty_buffer.

    TYPES: tty_buffer TYPE TABLE OF ty_buffer WITH KEY object_type
                                                       object_name.

    CONSTANTS max_entries TYPE i VALUE 100.

    CLASS-DATA buffer TYPE tty_buffer.

    CLASS-METHODS get_from_buffer IMPORTING object_type   TYPE trobjtype
                                                object_name   TYPE sobj_name
                                      RETURNING VALUE(result) TYPE abap_bool
                                      RAISING   cx_sy_itab_line_not_found.

    CLASS-METHODS try_new_exemption IMPORTING object_type   TYPE trobjtype
                                        object_name   TYPE sobj_name
                              RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_exemption_handler IMPLEMENTATION.


  METHOD y_if_exemption~is_object_exempted.
    TRY.
        result = get_from_buffer( object_type  = object_type
                                            object_name  = object_name ).
      CATCH cx_sy_itab_line_not_found.
        result = try_new_exemption( object_type = object_type
                                    object_name = object_name ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_from_buffer.
    DATA(entry) = buffer[ object_type = object_type
                          object_name = object_name ].
    result = entry-is_exempted.
  ENDMETHOD.


  METHOD try_new_exemption.
    IF lines( buffer ) > max_entries.
      DELETE buffer FROM 1 TO max_entries / 2.
    ENDIF.

    result = y_exemption_general=>is_object_exempted( object_type = object_type
                                                      object_name = object_name ).

    IF result = abap_false.
      result = COND #( WHEN object_type = 'PROG' THEN y_exemption_of_program=>is_exempted( object_name )
                       WHEN object_type = 'CLAS' THEN y_exemption_of_class=>is_exempted( object_name )
                       WHEN object_type = 'FUGR' THEN y_exemption_of_function_group=>is_exempted( object_name ) ).
    ENDIF.

    APPEND VALUE #( object_type = object_type
                    object_name = object_name
                    is_exempted = result ) TO buffer.
  ENDMETHOD.


ENDCLASS.
