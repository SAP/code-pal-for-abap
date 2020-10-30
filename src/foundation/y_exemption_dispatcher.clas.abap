CLASS y_exemption_dispatcher DEFINITION PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES y_if_exemption_dispatcher.
      ALIASES create FOR y_if_exemption_dispatcher~create.

  PRIVATE SECTION.
    METHODS get_exemption_from_database IMPORTING object_type   TYPE trobjtype
                                                  object_name   TYPE sobj_name
                                        RETURNING VALUE(result) TYPE abap_bool
                                        RAISING   ycx_entry_not_found.

    METHODS store_exemption_in_database IMPORTING exemption TYPE ytab_exemptions.

    METHODS is_dataset_outdated IMPORTING storedate     TYPE d
                                RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_exempted IMPORTING object_type   TYPE trobjtype
                                  object_name   TYPE sobj_name
                        RETURNING VALUE(result) TYPE abap_bool.

    METHODS try_new_exemption IMPORTING object_type   TYPE trobjtype
                                        object_name   TYPE sobj_name
                              RETURNING value(result) TYPE abap_bool.

ENDCLASS.



CLASS y_exemption_dispatcher IMPLEMENTATION.


  METHOD create.
    result = NEW y_exemption_dispatcher( ).
  ENDMETHOD.


  METHOD get_exemption_from_database.

    SELECT SINGLE *
    FROM ytab_exemptions
    INTO @DATA(exemption)
    WHERE object = @object_type
    AND obj_name = @object_name.

    IF sy-subrc IS NOT INITIAL
    OR is_dataset_outdated( exemption-as4date ) = abap_true.
      RAISE EXCEPTION TYPE ycx_entry_not_found.
      RETURN.
    ENDIF.

    result = exemption-is_exempted.

  ENDMETHOD.


  METHOD is_dataset_outdated.
    DATA(compare_date) = storedate + 14.
    result = xsdbool( compare_date < sy-datum ).
  ENDMETHOD.


  METHOD store_exemption_in_database.
    INSERT INTO ytab_exemptions VALUES @exemption.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    UPDATE ytab_exemptions SET as4date = @exemption-as4date,
                               is_exempted = @exemption-is_exempted
                           WHERE object = @exemption-object
                             AND obj_name = @exemption-obj_name.

    ASSERT sy-subrc = 0.
  ENDMETHOD.


  METHOD y_if_exemption_dispatcher~is_class_exempted.
    is_exempted = is_exempted( object_type = 'CLAS'
                               object_name = name ).
  ENDMETHOD.


  METHOD y_if_exemption_dispatcher~is_function_group_exempted.
    is_exempted = is_exempted( object_type = 'FUGR'
                               object_name = name ).
  ENDMETHOD.


  METHOD y_if_exemption_dispatcher~is_program_exempted.
    is_exempted = is_exempted( object_type = 'PROG'
                               object_name = name ).
  ENDMETHOD.


  METHOD is_exempted.
    TRY.
        result = get_exemption_from_database( object_type  = object_type
                                              object_name  = object_name ).
      CATCH ycx_entry_not_found.
        result = try_new_exemption( object_type = object_type
                                    object_name = object_name ).
    ENDTRY.
  ENDMETHOD.


  METHOD try_new_exemption.

    result = COND #( WHEN object_type = 'PROG' THEN y_exemption_of_program=>create( )->is_exempted( object_name )
                     WHEN object_type = 'CLAS' THEN y_exemption_of_class=>create( )->is_exempted( object_name )
                     WHEN object_type = 'FUGR' THEN y_exemption_of_function_group=>create( )->is_exempted( object_name ) ).

    IF result = abap_false.
      result = y_exemption_general=>create( )->is_object_exempted( object_type  = object_type
                                                                   object_name  = object_name ).
    ENDIF.

    DATA(exemption) = VALUE ytab_exemptions( object      = object_type
                                             obj_name    = object_name
                                             is_exempted = result
                                             as4date     = sy-datum ).

    store_exemption_in_database( exemption ).

  ENDMETHOD.



ENDCLASS.
