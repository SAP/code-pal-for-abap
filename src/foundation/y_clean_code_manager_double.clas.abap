CLASS y_clean_code_manager_double DEFINITION PUBLIC INHERITING FROM y_clean_code_manager.
  PUBLIC SECTION.
    METHODS constructor IMPORTING check_class TYPE REF TO y_check_base.

    METHODS y_if_clean_code_manager~calculate_obj_creation_date REDEFINITION.
    METHODS y_if_clean_code_manager~is_profile_in_use REDEFINITION.

  PRIVATE SECTION.
    DATA check_class TYPE REF TO y_check_base.

ENDCLASS.



CLASS y_clean_code_manager_double IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    me->check_class = check_class.
  ENDMETHOD.

  METHOD y_if_clean_code_manager~calculate_obj_creation_date.
    result = check_class->settings-object_created_on.
  ENDMETHOD.


  METHOD y_if_clean_code_manager~is_profile_in_use.
    result = abap_false.
  ENDMETHOD.


ENDCLASS.
