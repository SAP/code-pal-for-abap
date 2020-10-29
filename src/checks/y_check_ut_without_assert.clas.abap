CLASS y_check_ut_without_assert DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.


CLASS y_check_ut_without_assert IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC UT_ASSERT' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-apply_on_test_code = abap_true.
    settings-apply_on_productive_code = abap_false.
    settings-documentation = |{ c_docs_path-checks }unit-test-without-assert.md|.

    set_check_message( 'Unit Tests must have assertions!' ).
  ENDMETHOD.

  METHOD inspect_tokens.

    CHECK get_token_rel( statement-from ) = 'METHOD'.

    DATA(configuration) = detect_check_configuration( statement ).

    IF configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from  = statement-from
                 error_priority  = configuration-prio ).

  ENDMETHOD.


ENDCLASS.
