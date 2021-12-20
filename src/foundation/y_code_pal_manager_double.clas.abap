CLASS y_code_pal_manager_double DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES y_if_code_pal_manager.
    METHODS constructor IMPORTING check TYPE REF TO y_code_pal_base.

  PROTECTED SECTION.
    ALIASES exemption FOR y_if_code_pal_manager~exemption.
    ALIASES statistics FOR y_if_code_pal_manager~statistics.
    ALIASES database_access FOR y_if_code_pal_manager~database_access.
    ALIASES creation_date FOR y_if_code_pal_manager~creation_date.
    ALIASES scope FOR y_if_code_pal_manager~scope.
    ALIASES profile FOR y_if_code_pal_manager~profile.

  PRIVATE SECTION.
    CONSTANTS srcid TYPE scr_source_id VALUE ''.
    DATA check TYPE REF TO y_code_pal_base.

ENDCLASS.



CLASS y_code_pal_manager_double IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    me->check = check.
    me->exemption = NEW ltd_exemption( ).
    me->statistics = NEW ltd_statistics( ).
    me->database_access = NEW y_code_pal_database_access( srcid ).
    me->creation_date = NEW ltd_creation_date( check ).
    me->scope = NEW ltd_scope( ).
    me->profile = NEW ltd_profile( ).
  ENDMETHOD.


  METHOD y_if_code_pal_manager~get_profile_configuration.
    result = VALUE #( ( apply_on_testcode        = check->settings-apply_on_test_code
                        apply_on_productive_code = check->settings-apply_on_productive_code
                        prio                     = check->settings-prio
                        threshold                = check->settings-threshold
                        object_creation_date     = check->settings-object_created_on ) ).
  ENDMETHOD.


  METHOD y_if_code_pal_manager~set_scope.
    RETURN.
  ENDMETHOD.

ENDCLASS.
