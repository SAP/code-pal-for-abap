CLASS y_exemption_handler DEFINITION  PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES y_if_exemption.
    ALIASES create FOR y_if_exemption~create.

  PRIVATE SECTION.
    METHODS get_exemption_from_buffer IMPORTING object_type   TYPE trobjtype
                                                object_name   TYPE sobj_name
                                      RETURNING VALUE(result) TYPE abap_bool
                                      RAISING   cx_sy_itab_line_not_found.

    METHODS try_new_exemption IMPORTING object_type   TYPE trobjtype
                                        object_name   TYPE sobj_name
                              RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS y_exemption_handler IMPLEMENTATION.


  METHOD y_if_exemption~create.
    result = NEW y_exemption_handler( ).
  ENDMETHOD.


  METHOD get_exemption_from_buffer.
    result = y_exemption_buffer=>get( object_type = object_type
                                      object_name = CONV #( object_name ) )-is_exempted.
  ENDMETHOD.


  METHOD y_if_exemption~is_object_exempted.
    TRY.
        result = get_exemption_from_buffer( object_type  = object_type
                                            object_name  = object_name ).
      CATCH cx_sy_itab_line_not_found.
        result = try_new_exemption( object_type = object_type
                                    object_name = object_name ).
    ENDTRY.
  ENDMETHOD.


  METHOD try_new_exemption.

    result = COND #( WHEN object_type = 'CLAS' THEN y_exemption_dispatcher=>create( )->is_class_exempted( object_name )
                     WHEN object_type = 'FUGR' THEN y_exemption_dispatcher=>create( )->is_function_group_exempted( object_name )
                     WHEN object_type = 'PROG' THEN y_exemption_dispatcher=>create( )->is_program_exempted( object_name ) ).

    DATA(exemption) = VALUE ytab_exemptions( object = object_type
                                             obj_name = object_name
                                             is_exempted = result
                                             as4date_co = sy-datum
                                             is_exemption_buffered = abap_true ).

    y_exemption_buffer=>modify( exemption ).

  ENDMETHOD.


ENDCLASS.
