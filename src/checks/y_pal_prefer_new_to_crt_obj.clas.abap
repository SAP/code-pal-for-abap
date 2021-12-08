CLASS y_pal_prefer_new_to_crt_obj DEFINITION PUBLIC INHERITING FROM y_code_pal_base CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .

  PROTECTED SECTION.
    METHODS inspect_tokens REDEFINITION.
    METHODS add_check_quickfix REDEFINITION.

ENDCLASS.



CLASS y_pal_prefer_new_to_crt_obj IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    settings-pseudo_comment = '"#EC PREF_NEW' ##NO_TEXT.
    settings-disable_threshold_selection = abap_true.
    settings-threshold = 0.
    settings-documentation = |{ c_docs_path-checks }prefer-new-to-create-object.md|.

    set_check_message( 'Prefer NEW to CREATE OBJECT!' ).
  ENDMETHOD.


  METHOD inspect_tokens.
    CHECK get_token_abs( statement-from ) =  'CREATE'
      AND get_token_abs( statement-from + 1 ) = 'OBJECT'.

    CHECK get_token_abs( statement-to - 1 ) <> 'FOR'
      AND get_token_abs( statement-to ) <> 'TESTING'.

    CHECK next1( 'TYPE' ) NA '()'.

    DATA(check_configuration) = detect_check_configuration( statement ).

    raise_error( statement_level = statement-level
                 statement_index = index
                 statement_from = statement-from
                 check_configuration = check_configuration ).
  ENDMETHOD.


  METHOD add_check_quickfix.
    RETURN.
  ENDMETHOD.

ENDCLASS.
