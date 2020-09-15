class Y_CHECK_EQUALS_SIGN_CHAINING definition
  public
  inheriting from Y_CHECK_BASE
  create public .

public section.

  methods CONSTRUCTOR .
  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Y_CHECK_EQUALS_SIGN_CHAINING IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC EQUALS_CHAINING'.
    settings-documentation = |{ c_docs_path-checks }equals-sign-chaining.md|.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.

    set_check_message( 'Values should not be allocated more than once within one statement!' ).
  ENDMETHOD.


  METHOD inspect_tokens.

    CHECK get_token_abs( statement-from + 1 ) EQ '='.
    CHECK get_token_abs( statement-from + 3 ) EQ '='.

    DATA(check_configuration) = detect_check_configuration( statement ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio ).

  ENDMETHOD.
ENDCLASS.
