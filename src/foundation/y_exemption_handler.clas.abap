CLASS y_exemption_handler DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS is_object_exempted
      IMPORTING
        !object_name  TYPE sobj_name
        !object_type  TYPE trobjtype
      RETURNING
        VALUE(result) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_exemption_from_buffer
      IMPORTING
        !object_type  TYPE trobjtype
        !object_name  TYPE sobj_name
      EXPORTING
        !is_exempted  TYPE abap_bool
        !is_in_buffer TYPE abap_bool .
    METHODS insert_exemption_into_buffer
      IMPORTING
        !object_type     TYPE trobjtype
        !object_name     TYPE sobj_name
        !is_exempted     TYPE abap_bool
      RETURNING
        VALUE(result) TYPE abap_bool .
ENDCLASS.



CLASS Y_EXEMPTION_HANDLER IMPLEMENTATION.


  METHOD get_exemption_from_buffer.
    SELECT SINGLE is_exempted FROM ytab_exemptions INTO @is_exempted
      WHERE object                = @object_type AND
            obj_name              = @object_name AND
            is_exemption_buffered = @abap_true.
    IF sy-subrc = 0.
      is_in_buffer = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD insert_exemption_into_buffer.
    DATA: exemption TYPE ytab_exemptions.

    exemption-object = object_type.
    exemption-obj_name = object_name.
    exemption-is_exempted = is_exempted.
    exemption-as4date_co = sy-datum.
    exemption-is_exemption_buffered = abap_true.

    INSERT ytab_exemptions FROM exemption.
    IF sy-subrc = 0.
      result = abap_true.
    ELSE.
      UPDATE ytab_exemptions SET is_exempted = @is_exempted,
                                 as4date_co = @sy-datum,
                                 is_exemption_buffered = @abap_true
       WHERE object = @object_type AND obj_name = @object_name.
      IF sy-subrc = 0.
        result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD is_object_exempted.
    get_exemption_from_buffer( EXPORTING
                                 object_type  = object_type
                                 object_name  = object_name
                               IMPORTING
                                 is_exempted = result
                                 is_in_buffer = DATA(is_in_buffer) ).
    IF is_in_buffer = abap_true.
      RETURN.
    ENDIF.

    DATA(generic_exemptor) = NEW y_exemption_dispatcher( ).
    CASE object_type.
      WHEN 'CLAS'.
        result = xsdbool( NEW y_exemption_of_class( )->y_if_exemption_of_objects~is_exempted( object_name ) = abap_true OR
                          generic_exemptor->y_if_exemption_dispatcher~is_class_exempted( object_name ) = abap_true ).

      WHEN 'FUGR'.
        result = xsdbool( NEW y_exemption_of_function_group( )->y_if_exemption_of_objects~is_exempted( object_name ) = abap_true OR
                          generic_exemptor->y_if_exemption_dispatcher~is_function_group_exempted( object_name ) = abap_true ).

      WHEN 'PROG'.
        result = xsdbool( NEW y_exemption_of_program( )->y_if_exemption_of_objects~is_exempted( object_name ) = abap_true OR
                          generic_exemptor->y_if_exemption_dispatcher~is_program_exempted( object_name ) = abap_true ).
      WHEN OTHERS.
    ENDCASE.

    IF result EQ abap_false.
      result =  NEW y_exemption_general( )->is_object_exempted( object_type  = object_type
                                                                object_name  = object_name ).
    ENDIF.

    DATA(is_stored) = insert_exemption_into_buffer( object_type = object_type
                                                    object_name = object_name
                                                    is_exempted = result ).
  ENDMETHOD.
ENDCLASS.
