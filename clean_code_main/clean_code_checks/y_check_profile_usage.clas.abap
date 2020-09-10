CLASS y_check_profile_usage DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS Y_CHECK_PROFILE_USAGE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    description = 'Profile Usage'(001).
    category    = 'Y_CHECK_CATEGORY'.
    version     = '0000'.
    position    = '740'.
    has_documentation = abap_true.

    settings-disable_on_testcode_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_test_code = abap_true.
    settings-apply_on_productive_code = abap_true.
    settings-prio = c_note.

    y_message_registration=>add_message(
      EXPORTING
        check_name = me->myname
        text       = '[Clean Code]: Profile is Active'(102)
      CHANGING
        messages   = me->scimessages ).
  ENDMETHOD.


  METHOD execute_check.

    CHECK has_attributes = abap_false.

    DATA(check_configuration) = detect_check_configuration( VALUE #( level = 1 ) ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = 1
                 statement_index     = 1
                 statement_from      = 1
                 error_priority      = check_configuration-prio ).

  ENDMETHOD.


  METHOD inspect_tokens.
    RETURN.
  ENDMETHOD.


ENDCLASS.
