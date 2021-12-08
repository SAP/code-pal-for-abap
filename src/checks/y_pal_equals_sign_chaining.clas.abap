CLASS y_pal_equals_sign_chaining DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

  PRIVATE SECTION.
    METHODS get_second_token IMPORTING statement     TYPE sstmnt
                             RETURNING VALUE(result) TYPE string.

    METHODS get_fourth_token IMPORTING statement     TYPE sstmnt
                             RETURNING VALUE(result) TYPE string.

ENDCLASS.



CLASS Y_PAL_EQUALS_SIGN_CHAINING IMPLEMENTATION.


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

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD get_fourth_token.
    CHECK statement-from + 3 < statement-to. "#EC CI_MAGIC
    result = get_token_abs( statement-from + 3 ).
  ENDMETHOD.


  METHOD get_second_token.
    CHECK statement-from + 1 < statement-to.
    result = get_token_abs( statement-from + 1 ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
