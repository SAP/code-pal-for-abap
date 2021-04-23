CLASS y_check_test_seam_usage DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

ENDCLASS.



CLASS Y_CHECK_TEST_SEAM_USAGE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC TEST_SEAM_USAGE' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }test-seam-usage.md|.

    set_check_message( '"TEST-SEAM" statement should no longer be used!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) = 'TEST-SEAM'.

    DATA(check_configuration) = detect_check_configuration( statement ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = check_configuration-prio ).
  ENDMETHOD.
ENDCLASS.
