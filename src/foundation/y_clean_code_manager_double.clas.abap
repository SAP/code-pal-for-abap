CLASS y_clean_code_manager_double DEFINITION PUBLIC INHERITING FROM y_clean_code_manager.
  PUBLIC SECTION.
    METHODS constructor IMPORTING check_class TYPE REF TO y_check_base.
    METHODS read_check_customizing REDEFINITION.
    METHODS calculate_obj_creation_date REDEFINITION.
  PRIVATE SECTION.
    DATA check_class TYPE REF TO y_check_base.
ENDCLASS.



CLASS y_clean_code_manager_double IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    me->check_class = check_class.
  ENDMETHOD.


  METHOD read_check_customizing.
    result = VALUE #( ( apply_on_testcode        = check_class->settings-apply_on_test_code
                        apply_on_productive_code = check_class->settings-apply_on_productive_code
                        prio                     = check_class->settings-prio
                        threshold                = check_class->settings-threshold ) ).
  ENDMETHOD.


  METHOD calculate_obj_creation_date.
    result = check_class->settings-object_created_on.
  ENDMETHOD.


ENDCLASS.
