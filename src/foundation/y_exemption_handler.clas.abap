CLASS y_exemption_handler DEFINITION  PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES y_if_exemption.
    ALIASES create FOR y_if_exemption~create.

  PRIVATE SECTION.
    METHODS get_exemption_from_buffer IMPORTING object_type  TYPE trobjtype
                                                object_name  TYPE sobj_name
                                      RETURNING VALUE(result) TYPE abap_bool
                                      RAISING ycx_entry_not_found.

    METHODS insert_exemption_into_buffer IMPORTING exemption TYPE ytab_exemptions.

    METHODS try_new_exemption IMPORTING object_type TYPE trobjtype
                                        object_name TYPE sobj_name
                              RETURNING value(result) TYPE abap_bool.

ENDCLASS.



CLASS Y_EXEMPTION_HANDLER IMPLEMENTATION.


  METHOD y_if_exemption~create.
    result = NEW y_exemption_handler( ).
  ENDMETHOD.


  METHOD get_exemption_from_buffer.
    DATA(exemption) = y_exemption_buffer=>get( object_type = object_type
                                               object_name = CONV #( object_name ) ).
                                               
    result = exemption-is_exempted.
  ENDMETHOD.


  METHOD insert_exemption_into_buffer.
    DATA exemption TYPE ytab_exemptions.

    exemption-object = object_type.
    exemption-obj_name = object_name.
    exemption-is_exempted = is_exempted.
    exemption-as4date_co = sy-datum.
    exemption-is_exemption_buffered = abap_true.

    y_exemption_buffer=>modify( exemption ).
  ENDMETHOD.


  METHOD y_if_exemption~is_object_exempted.
    TRY.
        result = get_exemption_from_buffer( object_type  = object_type
                                            object_name  = object_name ).
      CATCH ycx_entry_not_found.
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

    insert_exemption_into_buffer( exemption ).

  ENDMETHOD.


ENDCLASS.
