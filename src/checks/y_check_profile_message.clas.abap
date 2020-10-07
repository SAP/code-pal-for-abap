CLASS y_check_profile_message DEFINITION
  PUBLIC
  INHERITING FROM y_check_base
  CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .
  PROTECTED SECTION.
    METHODS execute_check REDEFINITION.
    METHODS inspect_tokens REDEFINITION.
ENDCLASS.

CLASS y_check_profile_message IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    has_documentation = abap_false.

    settings-disable_on_testcode_selection = abap_true.
    settings-disable_on_prodcode_selection = abap_true.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_test_code = abap_true.
    settings-apply_on_productive_code = abap_true.
    settings-prio = c_note.

    enable_run_only_once( ).

    set_check_message( 'code pal for ABAP Profile is being used.' ).
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
