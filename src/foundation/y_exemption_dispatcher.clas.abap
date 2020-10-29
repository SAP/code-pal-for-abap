CLASS y_exemption_dispatcher DEFINITION PUBLIC CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES y_if_exemption_dispatcher.

  PRIVATE SECTION.
    METHODS get_exemption_from_database
      IMPORTING
        object_type  TYPE trobjtype
        object_name  TYPE sobj_name
      EXPORTING
        is_exempted  TYPE abap_bool
        is_in_buffer TYPE abap_bool.

    METHODS store_exemption_in_database
      IMPORTING
        exemption TYPE ytab_exemptions.

    METHODS is_dataset_outdated
      IMPORTING
        storedate     TYPE d
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS is_exempt
      IMPORTING
        object_type  TYPE trobjtype
        object_name  TYPE sobj_name
      returning
        value(result) type abap_bool.

ENDCLASS.



CLASS y_exemption_dispatcher IMPLEMENTATION.


  METHOD get_exemption_from_database.
    is_in_buffer = abap_true.
    SELECT SINGLE is_exempted, as4date FROM ytab_exemptions
        INTO ( @is_exempted, @DATA(storedate) )
        WHERE object = @object_type AND obj_name = @object_name.
    IF sy-subrc = 4.
      is_in_buffer = abap_false.
      RETURN.
    ENDIF.

    IF is_dataset_outdated( storedate ).
      is_in_buffer = abap_false.
    ENDIF.
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

    UPDATE ytab_exemptions SET as4date = @sy-datum,
                               is_exempted = @exemption-is_exempted
                           WHERE object = @exemption-object
                             AND obj_name = @exemption-obj_name.

    ASSERT sy-subrc = 0.
  ENDMETHOD.


  METHOD y_if_exemption_dispatcher~is_class_exempted.
    is_exempted = is_exempt( object_type = 'CLAS'
                             object_name = name ).
  ENDMETHOD.


  METHOD y_if_exemption_dispatcher~is_function_group_exempted.
    is_exempted = is_exempt( object_type = 'FUGR'
                             object_name = name ).
  ENDMETHOD.


  METHOD y_if_exemption_dispatcher~is_program_exempted.
    is_exempted = is_exempt( object_type = 'PROG'
                             object_name = name ).
  ENDMETHOD.


  METHOD is_exempt.
     get_exemption_from_database(
        EXPORTING
          object_type  = object_type
          object_name  = object_name
        IMPORTING
          is_exempted = DATA(is_exempt)
          is_in_buffer = DATA(is_in_buffer)
    ).

    IF is_in_buffer = abap_true.
      result = is_exempt.
      RETURN.
    ENDIF.

    result = COND #( WHEN object_type = 'PROG' THEN y_exemption_of_program=>create( )->is_exempted( object_name )
                     WHEN object_type = 'CLAS' THEN y_exemption_of_class=>create( )->is_exempted( object_name )
                     WHEN object_type = 'FUGR' THEN y_exemption_of_function_group=>create( )->is_exempted( object_name ) ).

    DATA(exemption) = VALUE ytab_exemptions( object      = object_type
                                             obj_name    = object_name
                                             is_exempted = result
                                             as4date     = sy-datum ).

    store_exemption_in_database( exemption ).
  ENDMETHOD.

ENDCLASS.
