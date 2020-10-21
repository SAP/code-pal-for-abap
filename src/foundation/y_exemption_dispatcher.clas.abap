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
ENDCLASS.



CLASS Y_EXEMPTION_DISPATCHER IMPLEMENTATION.


  METHOD get_exemption_from_database.
    DATA(exemption) = y_exemption_buffer=>get( object_type = object_type
                                               object_name = CONV #( object_name ) ).

    is_in_buffer = xsdbool( exemption IS NOT INITIAL ).
    is_exempted = exemption-is_exempted.
  ENDMETHOD.


  METHOD store_exemption_in_database.
    DATA(line) = VALUE ytab_exemptions( object      = object_type
                                        obj_name    = object_name
                                        is_exempted = is_exempted
                                        as4date     = sy-datum ).
    y_exemption_buffer=>modify( line ).
    is_stored = abap_true.
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
