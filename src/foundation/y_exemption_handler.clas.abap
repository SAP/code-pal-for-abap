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
    SELECT SINGLE is_exempted
    FROM ytab_exemptions
    INTO @result
    WHERE object = @object_type
    AND obj_name = @object_name
    AND is_exemption_buffered = @abap_true.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE ycx_entry_not_found.
    ENDIF.
  ENDMETHOD.


  METHOD insert_exemption_into_buffer.
    INSERT ytab_exemptions FROM exemption.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    UPDATE ytab_exemptions SET is_exempted = @exemption-is_exempted,
                               as4date_co = @exemption-as4date_co,
                               is_exemption_buffered = @exemption-is_exemption_buffered
                           WHERE object = @exemption-object
                           AND obj_name = @exemption-obj_name.

    ASSERT sy-subrc = 0.
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
