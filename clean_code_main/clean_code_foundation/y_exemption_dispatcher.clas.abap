CLASS y_exemption_dispatcher DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES y_if_exemption_dispatcher.

protected section.
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
        object_type      TYPE trobjtype
        object_name      TYPE sobj_name
        is_exempted      TYPE abap_bool
      RETURNING
        VALUE(is_stored) TYPE abap_bool.

    METHODS is_dataset_outdated
      IMPORTING
        storedate          TYPE d
      RETURNING
        VALUE(is_outdated) TYPE abap_bool.
ENDCLASS.



CLASS Y_EXEMPTION_DISPATCHER IMPLEMENTATION.


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
    is_outdated = xsdbool( compare_date < sy-datum ) .
  ENDMETHOD.


  METHOD store_exemption_in_database.
    DATA(line) = VALUE ytab_exemptions( object      = object_type
                                        obj_name    = object_name
                                        is_exempted = is_exempted
                                        as4date     = sy-datum ).
    INSERT INTO ytab_exemptions VALUES @line.

    IF sy-subrc = 0.
      is_stored = abap_true.
    ELSE.
      UPDATE ytab_exemptions SET as4date = @sy-datum,
                                 is_exempted = @is_exempted
       WHERE object = @object_type AND obj_name = @object_name.
      IF sy-subrc = 0.
        is_stored = abap_true.
      ELSE.
        ASSERT sy-subrc <> 0.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD y_if_exemption_dispatcher~is_class_exempted.
    get_exemption_from_database(
       EXPORTING
         object_type  = 'CLAS'
         object_name  =  name
       IMPORTING
         is_exempted = is_exempted
         is_in_buffer = DATA(is_in_buffer) ).
    IF is_in_buffer = abap_true.
      RETURN.
    ENDIF.

    is_exempted = NEW y_exemption_of_class( )->y_if_exemption_of_objects~is_exempted( name ).

    ASSERT store_exemption_in_database( object_type = 'CLAS'
                                        object_name = name
                                        is_exempted = is_exempted ).
  ENDMETHOD.


  METHOD y_if_exemption_dispatcher~is_function_group_exempted.
    get_exemption_from_database(
        EXPORTING
          object_type  = 'FUGR'
          object_name  =  name
        IMPORTING
          is_exempted = is_exempted
          is_in_buffer = DATA(is_in_buffer) ).
    IF is_in_buffer = abap_true.
      RETURN.
    ENDIF.

    is_exempted = NEW y_exemption_of_function_group( )->y_if_exemption_of_objects~is_exempted( name ).

    ASSERT store_exemption_in_database( object_type = 'FUGR'
                                        object_name = name
                                        is_exempted = is_exempted ).
  ENDMETHOD.


  METHOD y_if_exemption_dispatcher~is_program_exempted.
    get_exemption_from_database(
        EXPORTING
          object_type  = 'PROG'
          object_name  =  name
        IMPORTING
          is_exempted = is_exempted
          is_in_buffer = DATA(is_in_buffer) ).
    IF is_in_buffer = abap_true.
      RETURN.
    ENDIF.

    is_exempted = NEW y_exemption_of_program( )->y_if_exemption_of_objects~is_exempted( name ).

    ASSERT store_exemption_in_database( object_type = 'PROG'
                                        object_name = name
                                        is_exempted = is_exempted ).
  ENDMETHOD.
ENDCLASS.
