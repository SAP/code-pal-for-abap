CLASS y_check_equals_sign_chaining DEFINITION PUBLIC INHERITING FROM y_check_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.

  PRIVATE SECTION.
    METHODS get_second_token IMPORTING statement     TYPE sstmnt
                             RETURNING VALUE(result) TYPE string.

    METHODS get_fourth_token IMPORTING statement     TYPE sstmnt
                             RETURNING VALUE(result) TYPE string.

ENDCLASS.



CLASS y_check_equals_sign_chaining IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC EQUALS_CHAINING'.
    settings-documentation = |{ c_docs_path-checks }equals-sign-chaining.md|.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.

    set_check_message( 'Values should not be allocated more than once within one statement!' ).
  ENDMETHOD.


  METHOD inspect_tokens.

    CHECK get_second_token( statement ) = '='.
    CHECK get_fourth_token( statement ) = '='.

    DATA(check_configuration) = detect_check_configuration( statement ).

    IF check_configuration IS INITIAL.
      RETURN.
    ENDIF.

    raise_error( statement_level     = statement-level
                 statement_index     = index
                 statement_from      = statement-from
                 error_priority      = check_configuration-prio ).

  ENDMETHOD.

  METHOD get_fourth_token.
    result = get_token_abs( statement-from + 3 ).
  ENDMETHOD.

  METHOD get_second_token.
    result = get_token_abs( statement-from + 1 ).
  ENDMETHOD.
ENDCLASS.
